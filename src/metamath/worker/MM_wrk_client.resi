let beginWorkerInteraction: (
    ~procName: string,
    ~initialRequest: 'req,
    ~onResponse: (~resp: 'resp, ~sendToWorker:'req=>unit, ~endWorkerInteraction: unit => unit) => unit,
    ~enableTrace: bool=?,
    ~reqToStr:'req=>string =?,
    ~respToStr:'resp=>string =?,
    ()
) => unit

let terminateWorker: unit => unit