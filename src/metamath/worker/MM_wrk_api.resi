type serialized

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

let serialize: 'a => serialized

let deserialize: serialized => 'a

let logClientIsSendingRequest: (
    ~clientId:int,
    ~procName:string,
    ~req:'req,
    ~reqToStr:option<'req=>string>,
) => unit

let logClientReceivedResponse: (
    ~clientId:int,
    ~procName:string,
    ~resp:'resp,
    ~respToStr:option<'resp=>string>,
) => unit