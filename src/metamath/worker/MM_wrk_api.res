open Common

type serialized = Js.Json.t

type workerRequest = {
    clientId: int,
    procName: string,
    body: serialized,
    traceEnabled: bool,
}

type workerResponse = {
    clientId: int,
    body: serialized
}

external serialize: 'a => serialized = "%identity"
external deserialize: serialized => 'a = "%identity"

let logClientIsSendingRequest = (
    ~clientId:int,
    ~procName:string,
    ~req:'req,
    ~reqToStr:option<'req=>string>,
):unit => {
    let reqStr = reqToStr->Belt_Option.map(f => f(req))->Belt.Option.getWithDefault("")
    Js.Console.log(`${currTimeStr()} [${clientId->Belt_Int.toString},${procName}] C~> ${reqStr}`)
}

let logClientReceivedResponse = (
    ~clientId:int,
    ~procName:string,
    ~resp:'resp,
    ~respToStr:option<'resp=>string>,
):unit => {
    let respStr = respToStr->Belt_Option.map(f => f(resp))->Belt.Option.getWithDefault("")
    Js.Console.log(`${currTimeStr()} [${clientId->Belt_Int.toString},${procName}] ->C ${respStr}`)
}

let logWorkerIsSendingResponse = (
    ~clientId:int,
    ~procName:string,
    ~resp:'resp,
    ~respToStr:'resp=>string,
):unit => {
    Js.Console.log(`${currTimeStr()} [${clientId->Belt_Int.toString},${procName}] W~> ${respToStr(resp)}`)
}

let logWorkerReceivedRequest = (
    ~clientId:int,
    ~procName:string,
    ~req:'req,
    ~reqToStr:'req=>string,
):unit => {
    Js.Console.log(`${currTimeStr()} [${clientId->Belt_Int.toString},${procName}] ->W ${reqToStr(req)}`)
}