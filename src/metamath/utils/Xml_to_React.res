open Expln_React_Mui
open Expln_React_common

let validateValue = ( ~value:string, ~allowedValues:array<string>, ~valueType:string, ): result<unit,string> => {
    if (allowedValues->Js_array2.includes(value)) {
        Ok(())
    } else {
        Error(`${valueType} "${value}" is not supported.`)
    }
}

let allowedTags = ["div", "span", "ol", "ul", "li", "pre", "table", "tbody", "tr", "td", "a"]
let styleAttrs = ["font-weight", "color"]
let otherAttrs = ["href"]
let supportedAttrs = styleAttrs->Js_array2.concat(otherAttrs)

let validateTagName = tagName => validateValue(~value=tagName, ~allowedValues=allowedTags, ~valueType="Tag")
let validateAttrName = attrName => validateValue(~value=attrName, ~allowedValues=supportedAttrs, ~valueType="Attribute")

let isIncludedIntoAllowedValues = (
    ~value:string,
    ~allowedValues:array<string>,
    ~attrName:string,
): result<unit,string> => {
    if (allowedValues->Js_array2.includes(value)) {
        Ok(())
    } else {
        Error(`Value of "${value}" is not supported by "${attrName}" attribute.`)
    }
}

let validateAttr = (attrName:string, attrValue:string): result<unit,string> => {
    switch validateAttrName(attrName) {
        | Error(msg) => Error(msg)
        | Ok(_) => {
            switch attrName {
                | "font-weight" => isIncludedIntoAllowedValues(~value=attrValue, ~attrName, ~allowedValues=["normal","bold"])
                | "color" => isIncludedIntoAllowedValues(~value=attrValue, ~attrName, 
                    ~allowedValues=["aqua", "black", "blue", "fuchsia", "gray", "green", "lime", "maroon", "navy", "olive", 
                                    "purple", "red", "silver", "teal", "white", "yellow"])
                | "href" => {
                    if (attrValue->Js_string2.startsWith("http://") || attrValue->Js_string2.startsWith("https://")) {
                        Ok(())
                    } else {
                        Error(`A link url should start with http or https, but got: ${attrValue}`)
                    }
                }
                | _ => Error(`Attribute "${attrName}" is not supported [2].`)
            }
        }
    }
}

let createStyle = (attrs:Belt_MapString.t<string>):option<ReactDOM.style> => {
    if (attrs->Belt_MapString.findFirstBy((attr,_) => styleAttrs->Js_array2.includes(attr))->Belt.Option.isNone) {
        None
    } else {
        Some(ReactDOM.Style.make(
            ~fontWeight=?(attrs->Belt_MapString.get("font-weight")),
            ~color=?(attrs->Belt_MapString.get("color")),
            ()
        ))
    }
}

let createDomProps = (attrs:Belt_MapString.t<string>):option<ReactDOMRe.domProps> => {
    if (attrs->Belt_MapString.size == 0) {
        None
    } else {
        Some(ReactDOMRe.domProps(
            ~style=?createStyle(attrs),
            ~fontWeight=?(attrs->Belt_MapString.get("font-weight")),
            ~href=?(attrs->Belt_MapString.get("href")),
            ~title=?(attrs->Belt_MapString.get("href")),
            ()
        ))
    }
}

let xmlToReactElem = (xml:Xml_parser.xmlNode):result<reElem,string> => {
    let saveChild = (childrenStack:Belt_MutableStack.t<array<reElem>>, child:reElem):unit => {
        switch childrenStack->Belt_MutableStack.top {
            | None => Js.Exn.raiseError("childrenStack->Belt_MutableStack.top is None")
            | Some(children) => children->Js_array2.push(child)->ignore
        }
    }

    let (_, resOpt) = Expln_utils_data.traverseTree(
        Belt_MutableStack.make(),
        xml,
        (_,node) => {
            switch node {
                | Node({children}) => Some(children)
                | Text(_) => None
            }
        },
        ~process = (childrenStack, node) => {
            switch node {
                | Node(_) => childrenStack->Belt_MutableStack.push([])
                | Text(_) => ()
            }
            None
        },
        ~postProcess = (childrenStack,node) => {
            let thisElem = switch node {
                | Text(str) => Ok(str->React.string)
                | Node({name, attrs, children}) => {
                    switch validateTagName(name) {
                        | Error(msg) => Error(msg)
                        | Ok(_) => {
                            let attrValidation = attrs->Belt_MapString.reduce(
                                Ok(()),
                                (res,attrName,attrValue) => {
                                    switch res {
                                        | Error(_) => res
                                        | Ok(_) => validateAttr(attrName,attrValue)
                                    }
                                }
                            )
                            switch attrValidation {
                                | Error(msg) => Error(msg)
                                | Ok(_) => {
                                    let children = switch childrenStack->Belt_MutableStack.pop {
                                        | None => []
                                        | Some(children) => children
                                    }
                                    Ok(ReactDOMRe.createDOMElementVariadic(
                                        name, ~props=?createDomProps(attrs), children
                                    ))
                                }
                            }
                        }
                    }
                }
            }
            switch thisElem {
                | Error(msg) => Some(Error(msg))
                | Ok(thisElem) => {
                    if (childrenStack->Belt_MutableStack.size == 0) {
                        Some(Ok(thisElem))
                    } else {
                        childrenStack->saveChild(thisElem)
                        None
                    }
                }
            }
        },
        ()
    )

    switch resOpt {
        | None => Js.Exn.raiseError("Got None as a result of convertion from XML to React Elem.")
        | Some(res) => res
    }
}
