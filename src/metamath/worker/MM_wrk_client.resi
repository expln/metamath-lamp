let beginWorkerInteraction: (
    ~procName: string,
    ~initialRequest: 'req,
    ~onResponse: (~resp: 'resp, ~sendToWorker:'req=>unit, ~endWorkerInteraction: unit => unit) => unit,
    ~enableTrace: bool=?,
    ()
) => unit

let terminateWorker: unit => unit