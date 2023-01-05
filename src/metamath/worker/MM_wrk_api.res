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