open MM_wrk_client
open MM_parser

let procName = "MM_wrk_ParseMmFile"

type parseResult = result<(mmAstNode, array<string>), string>

type request = 
    | ParseMmFile({mmFileText: string})

type response =
    | MmFileParseProgress({pct: float})
    | MmFileParsed({parseResult: parseResult})

let reqToStr = req => {
    switch req {
        | ParseMmFile(_) => "ParseMmFile"
    }
}

let respToStr = resp => {
    switch resp {
        | MmFileParseProgress({pct}) => `MmFileParseProgress(pct=${pct->Belt_Float.toString})`
        | MmFileParsed(_) => `MmFileParsed`
    }
}

let beginParsingMmFile = (~mmFileText, ~onProgress:float=>unit, ~onDone:parseResult=>unit) => {
    beginWorkerInteraction(
        ~procName,
        ~initialRequest = ParseMmFile({mmFileText:mmFileText}),
        ~onResponse = (~resp:response, ~sendToWorker as _, ~endWorkerInteraction:unit=>unit) => {
            switch resp {
                | MmFileParseProgress({pct}) => onProgress(pct)
                | MmFileParsed({parseResult}) => {
                    endWorkerInteraction()
                    onDone(parseResult)
                }
            }
        },
        ()
    )
}

let processOnWorkerSide = (~req: request, ~sendToClient: response => unit): unit => {
    switch req {
        | ParseMmFile({mmFileText}) => {
            try {
                let parseResult = parseMmFile(
                    ~mmFileContent=mmFileText,
                    ~onProgress = pct => {
                        sendToClient(MmFileParseProgress({pct:pct}))
                    },
                    ()
                )
                sendToClient(MmFileParseProgress({pct:1.}))
                sendToClient(MmFileParsed({parseResult:Ok(parseResult)}))
            } catch {
                | MmException({msg}) => {
                    sendToClient(MmFileParsed({parseResult:Error(msg)}))
                }
            }
        }
    }
}
