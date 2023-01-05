open MM_wrk_api
open Common

let webworker: option<{..}> = %raw("typeof window !== 'undefined' ? window.webWorkerInst : undefined")
let sendToWorkerPriv: workerRequest => unit = req => {
    webworker->Belt_Option.forEach(webworker => webworker["postMessage"](. req))
}

type clientCallback = serialized => unit

type client = {
    id: int,
    callback: clientCallback,
    traceEnabled: bool,
}

let nextClientId = ref(0)

let getNextClientId = () => {
    nextClientId.contents = nextClientId.contents + 1
    nextClientId.contents - 1
}

let clients = []

let regClient = (~callback:clientCallback, ~enableTrace:bool) => {
    let id = getNextClientId()
    clients->Js_array2.push({ id, callback, traceEnabled:enableTrace })->ignore
    id
}

let unregClient = id => {
    let i = ref(0)
    while (i.contents < clients->Js_array2.length) {
        if (clients[i.contents].id == id) {
            clients->Js_array2.removeCountInPlace(~pos=i.contents, ~count=1)->ignore
        } else {
            i.contents = i.contents + 1
        }
    }
}

webworker->Belt_Option.forEach(webworker => {
    webworker["onmessage"]= msg => {
        let resp:workerResponse = msg["data"]
        clients->Expln_utils_common.arrForEach(client => {
            if (client.id == resp.clientId) {
                if (client.traceEnabled) {
                    Js.Console.log(`${currTimeStr()} [clientId=${resp.clientId->Belt_Int.toString}] client received a response`)
                }
                client.callback(resp.body)
                Some(())
            } else {
                None
            }
        })->ignore
    }
})

let beginWorkerInteraction = (
    ~procName:string,
    ~initialRequest:'req, 
    ~onResponse:(~resp:'resp, ~sendToWorker:'req=>unit, ~endWorkerInteraction:unit=>unit)=>unit,
    ~enableTrace: bool=false,
    ()
) => {
    let id = ref(-1)
    let localSendToWorker = ref(_=>())
    id.contents = regClient(~enableTrace, ~callback = respBody => {
        onResponse(
            ~resp=deserialize(respBody),
            ~sendToWorker=localSendToWorker.contents,
            ~endWorkerInteraction= _=>unregClient(id.contents)
        )
    })
    localSendToWorker.contents = req => {
        if (enableTrace) {
            Js.Console.log(`${currTimeStr()} [clientId=${id.contents->Belt_Int.toString}] client is sending a request, procName = ${procName}`)
        }
        sendToWorkerPriv({clientId:id.contents, procName, body:serialize(req), traceEnabled: enableTrace})
    }
    localSendToWorker.contents(initialRequest)
}
