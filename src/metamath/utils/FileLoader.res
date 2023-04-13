
@module("./FileLoader") external loadFilePriv: (string, (int,int)=>unit, string=>unit, unit=>unit) => unit = "loadFile"

let loadFile = (
    ~url:string,
    ~onProgress:option<(int,int)=>unit>=?,
    ~onReady:string=>unit,
    ~onError:option<unit=>unit>=?,
    ()
) => {
    loadFilePriv(
        url,
        onProgress->Belt_Option.getWithDefault((_,_) => ()),
        onReady,
        onError->Belt_Option.getWithDefault(() => ()),
    )
}