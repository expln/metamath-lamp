open MM_wrk_api

@new external newWorker: string => {..} = "Worker"

exception WorkerException(string)

let webworkerRef: ref<option<{..}>> = ref(None)

type clientCallback = serialized => unit

type client = {
    id: int,
    callback: clientCallback,
    traceEnabled: bool,
}

let clients = []

let terminateWorker = () => {
    switch webworkerRef.contents {
        | None => ()
        | Some(webworker) => {
            webworker["terminate"](.)
            webworkerRef.contents = None
            clients->Js.Array2.spliceInPlace(~pos=0, ~remove=clients->Array.length, ~add=[])->ignore
        }
    }
}

let nextClientId = ref(0)

let getNextClientId = () => {
    nextClientId.contents = nextClientId.contents + 1
    nextClientId.contents - 1
}

let regClient = (~callback:clientCallback, ~enableTrace:bool) => {
    let id = getNextClientId()
    clients->Array.push({ id, callback, traceEnabled:enableTrace })
    id
}

let unregClient = id => {
    let i = ref(0)
    while (i.contents < clients->Array.length) {
        if ((clients->Array.getUnsafe(i.contents)).id == id) {
            clients->Js_array2.removeCountInPlace(~pos=i.contents, ~count=1)->ignore
        } else {
            i.contents = i.contents + 1
        }
    }
}

let sendToWorkerPriv: workerRequest => unit = req => {
    if (webworkerRef.contents->Belt.Option.isNone) {
        let webworker = newWorker("./webworker-main.js")
        webworker["onmessage"]= msg => {
            let resp:workerResponse = msg["data"]
            clients->Expln_utils_common.arrForEach(client => {
                if (client.id == resp.clientId) {
                    client.callback(resp.body)
                    Some(())
                } else {
                    None
                }
            })->ignore
        }
        webworkerRef.contents = Some(webworker)
    }
    switch webworkerRef.contents {
        | None => raise(WorkerException(`Could not instantiate a webworker.`))
        | Some(webworker) => webworker["postMessage"](. req)
    }
}

let beginWorkerInteraction = (
    ~procName:string,
    ~initialRequest:'req, 
    ~onResponse:(~resp:'resp, ~sendToWorker:'req=>unit, ~endWorkerInteraction:unit=>unit)=>unit,
    ~enableTrace: bool=false,
    ~reqToStr:option<'req=>string>=?,
    ~respToStr:option<'resp=>string>=?,
    ()
) => {
    let id = ref(-1)
    let localSendToWorker = ref(_=>())
    id.contents = regClient(~enableTrace, ~callback = respBody => {
        let resp=deserialize(respBody)
        if (enableTrace) {
            logClientReceivedResponse( ~clientId=id.contents, ~procName, ~resp, ~respToStr, )
        }
        onResponse(
            ~resp,
            ~sendToWorker=localSendToWorker.contents,
            ~endWorkerInteraction= _=>unregClient(id.contents)
        )
    })
    localSendToWorker.contents = req => {
        if (enableTrace) {
            logClientIsSendingRequest( ~clientId=id.contents, ~procName, ~req, ~reqToStr, )
        }
        sendToWorkerPriv({clientId:id.contents, procName, body:serialize(req), traceEnabled: enableTrace})
    }
    localSendToWorker.contents(initialRequest)
}
