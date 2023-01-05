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
