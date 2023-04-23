type xmlAttr = {name: string, value: string}
type rec xmlNode = {name: string, attrs: array<xmlAttr>, children: array<xmlNode>}
let parseStr: string => result<xmlNode, string>
