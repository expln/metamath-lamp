type rec xmlNode =
    | Node({ name:string, attrs:Belt_MapString.t<string>, children: array<xmlNode>, })
    | Text(string)

let parseStr: string => result<xmlNode, string>
