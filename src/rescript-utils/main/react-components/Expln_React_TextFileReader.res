@new external createFileReader: unit => {..} = "FileReader"

let readFileContent = (file):promise<(string,string)> => {
    Promise.make((resolve,_) => {
        let fr = createFileReader()
        let fileName = file["name"]
        fr["onload"] = () => {
            let fileText = fr["result"]
            resolve((fileName, fileText))
        }
        fr["readAsBinaryString"](file)
    })
}

@react.component
let make = (~onChange:option<array<(string,string)>>=>unit) => {
    <input
        type_="file"
        multiple=true
        onChange={evt=>{
            let files = ReactEvent.Synthetic.nativeEvent(evt)["target"]["files"]
            if (files->Array.length == 0) {
                onChange(None)
            } else {
                Array.fromArrayLike(files)
                    ->Array.map(readFileContent)
                    ->Promise.all
                    ->Promise.thenResolve(fileContents => onChange(Some(fileContents)))
                    ->Promise.done
            }
        }}
    />
}