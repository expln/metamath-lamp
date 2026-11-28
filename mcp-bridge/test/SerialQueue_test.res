open Test

// A promise together with the function which resolves it.
let deferred = ():(promise<'a>, 'a => unit) => {
    let resolveRef = ref(_ => ())
    let promise = Promise.make((resolve, _) => resolveRef := resolve)
    (promise, resolveRef.contents)
}

// Lets all pending promise callbacks run.
let flush = ():promise<unit> => Promise.make((resolve, _) => setTimeout(() => resolve(), 0)->ignore)

// The task a test adds to a queue. It logs when it starts and finishes and returns the value
// passed to its `finish` function.
type testTask<'a> = {
    task: unit => promise<'a>,
    finish: 'a => unit,
}

let makeTestTask = (name:string, log:array<string>):testTask<'a> => {
    let (done, finish) = deferred()
    {
        task: async () => {
            log->Array.push(`${name} started`)
            let res = await done
            log->Array.push(`${name} finished`)
            res
        },
        finish,
    }
}

describe("SerialQueue.run", () => {
    itAsync("starts a task only after the previous one is finished", async () => {
        //given
        let log = []
        let q = SerialQueue.make()
        let a = makeTestTask("a", log)
        let b = makeTestTask("b", log)

        //when
        let aRes = q->SerialQueue.run(a.task)
        let bRes = q->SerialQueue.run(b.task)
        await flush()

        //then
        assertEq(log, ["a started"])

        //when
        a.finish(1)
        await flush()

        //then
        assertEq(log, ["a started", "a finished", "b started"])

        //when
        b.finish(2)

        //then
        assertEq([await aRes, await bRes], [1, 2])
        assertEq(log, ["a started", "a finished", "b started", "b finished"])
    })

    itAsync("runs tasks in the order they were added, even if later ones would finish sooner", async () => {
        //given
        let log = []
        let q = SerialQueue.make()
        let delayed = (name, delayMs) => async () => {
            log->Array.push(`${name} started`)
            await Promise.make((resolve, _) => setTimeout(() => resolve(), delayMs)->ignore)
            log->Array.push(`${name} finished`)
            name
        }

        //when
        let results = await Promise.all([
            q->SerialQueue.run(delayed("a", 30)),
            q->SerialQueue.run(delayed("b", 0)),
            q->SerialQueue.run(delayed("c", 10)),
        ])

        //then
        assertEq(results, ["a", "b", "c"])
        assertEq(log, ["a started", "a finished", "b started", "b finished", "c started", "c finished"])
    })

    itAsync("starts a task right away when the queue is idle", async () => {
        //given
        let log = []
        let q = SerialQueue.make()
        let a = makeTestTask("a", log)
        a.finish(1)
        await q->SerialQueue.run(a.task)->Promise.thenResolve(_ => ())
        let b = makeTestTask("b", log)

        //when
        let bRes = q->SerialQueue.run(b.task)
        await flush()

        //then
        assertEq(log, ["a started", "a finished", "b started"])
        b.finish(2)
        assertEq(await bRes, 2)
    })

    itAsync("passes a rejection of a task to the caller and runs the next task", async () => {
        //given
        let q = SerialQueue.make()
        let failing = async () => {
            await flush()
            JsError.throwWithMessage("task failed")
        }

        //when
        let failedRes = q->SerialQueue.run(failing)
        let nextRes = q->SerialQueue.run(async () => "next")

        //then
        let failedMsg = try {
            let _ = await failedRes
            "no error"
        } catch {
            | JsExn(err) => err->JsExn.message->Option.getOr("no message")
        }
        assertEq(failedMsg, "task failed")
        assertEq(await nextRes, "next")
    })

    itAsync("passes an error thrown synchronously by a task to the caller and runs the next task", async () => {
        //given
        let q = SerialQueue.make()
        let throwing = () => JsError.throwWithMessage("thrown synchronously")

        //when
        let failedRes = q->SerialQueue.run(throwing)
        let nextRes = q->SerialQueue.run(async () => "next")

        //then
        let failedMsg = try {
            let _ = await failedRes
            "no error"
        } catch {
            | JsExn(err) => err->JsExn.message->Option.getOr("no message")
        }
        assertEq(failedMsg, "thrown synchronously")
        assertEq(await nextRes, "next")
    })

    itAsync("keeps separate queues independent", async () => {
        //given
        let log = []
        let q1 = SerialQueue.make()
        let q2 = SerialQueue.make()
        let a = makeTestTask("a", log)
        let b = makeTestTask("b", log)

        //when
        let aRes = q1->SerialQueue.run(a.task)
        let bRes = q2->SerialQueue.run(b.task)
        await flush()

        //then
        assertEq(log, ["a started", "b started"])
        a.finish(1)
        b.finish(2)
        assertEq([await aRes, await bRes], [1, 2])
    })
})
