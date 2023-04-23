type xmlAttr = {
    name:string,
    value:string,
}

type rec xmlNode = {
    name:string,
    attrs:array<xmlAttr>,
    children: array<xmlNode>
}

type attr = {
    name: string,
    value: string,
}

type namedNodeMap = {
    length: int,
    item: (. int) => Js.Nullable.t<attr>
}

type element = {
    tagName: string,
    attributes: namedNodeMap,
}

type xmlDocument = { 
    documentElement: element
}

@new external newDomParser: unit => {..} = "DOMParser"

let parseXmlFromStr = (str:string):xmlDocument => {
    newDomParser()["parseFromString"](. str, "text/xml")
}

let attrToXmlAttr = (attr:attr):xmlAttr => {
    {
        name:attr.name,
        value:attr.value,
    }
}

let elementGetAttrs = (element:element):array<xmlAttr> => {
    let res = []
    let attrs = element.attributes
    for i in 0 to attrs.length-1 {
        switch attrs.item(. i)->Js.Nullable.toOption {
            | None => ()
            | Some(attr) => {
                res->Js_array2.push(attr->attrToXmlAttr)->ignore
            }
        }
    }
    res
}

let parseStr = (str:string):result<xmlNode,string> => {
    let doc = parseXmlFromStr(str)
    let root = doc.documentElement
    Ok({
        name:root.tagName,
        attrs:root->elementGetAttrs,
        children: []
    })
}