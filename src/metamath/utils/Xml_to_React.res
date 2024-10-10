open Expln_React_common

let validateValue = ( ~value:string, ~allowedValues:array<string>, ~valueType:string, ): result<unit,string> => {
    if (allowedValues->Array.includes(value)) {
        Ok(())
    } else {
        Error(`${valueType} "${value}" is not supported.`)
    }
}

let allowedTags = [
    "div", "p", "span", "ol", "ul", "li", "pre", 
    "table", "tbody", "thead", "tr", "td", 
    "a", "hr", "b", "i",
    "h1", "h2", "h3", "h4", "h5", "h6",
]
let styleAttrs = ["color", "font-weight", "font-family", "font-style", "font-size"]
let otherAttrs = ["href"]
let supportedAttrs = styleAttrs->Array.concat(otherAttrs)

let validateTagName = tagName => validateValue(~value=tagName, ~allowedValues=allowedTags, ~valueType="Tag")
let validateAttrName = attrName => validateValue(~value=attrName, ~allowedValues=supportedAttrs, ~valueType="Attribute")

let isIncludedIntoAllowedValues = (
    ~value:string,
    ~allowedValues:array<string>,
    ~attrName:string,
): result<unit,string> => {
    if (allowedValues->Array.includes(value)) {
        Ok(())
    } else {
        Error(
            `Value of "${value}" is not supported by "${attrName}" attribute. All the supported values are: ` 
            ++ allowedValues->Array.joinUnsafe(", ") ++ " ."
        )
    }
}

let fontSizeValuePattern = %re("/^\d+(.\d+)?em$/")

let validateAttr = (attrName:string, attrValue:string): result<unit,string> => {
    switch validateAttrName(attrName) {
        | Error(msg) => Error(msg)
        | Ok(_) => {
            switch attrName {
                | "font-weight" => isIncludedIntoAllowedValues(~value=attrValue, ~attrName, 
                    ~allowedValues=["normal","bold"])
                | "font-style" => isIncludedIntoAllowedValues(~value=attrValue, ~attrName, 
                    ~allowedValues=["normal","italic"])
                | "font-family" => isIncludedIntoAllowedValues(~value=attrValue, ~attrName, 
                    ~allowedValues=[ "serif", "sans-serif", "monospace", "cursive", "fantasy", "system-ui", "ui-serif", 
                        "ui-sans-serif", "ui-monospace", "ui-rounded", "emoji", "math", "fangsong", ])
                | "font-size" => {
                    switch attrValue->Js_string2.match_(fontSizeValuePattern) {
                        | None => Error(`"font-size" attribute should be specified using "em" units.`)
                        | Some(_) => Ok(())
                    }
                }
                | "color" => isIncludedIntoAllowedValues(~value=attrValue, ~attrName, 
                    ~allowedValues=["aqua", "black", "blue", "fuchsia", "gray", "green", "lime", "maroon", "navy", "olive", 
                                    "purple", "red", "silver", "teal", "white", "yellow"])
                | "href" => {
                    if (attrValue->String.startsWith("http://") || attrValue->String.startsWith("https://")) {
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

let createStyle = (attrs:Belt_MapString.t<string>, ~addBorder:bool):option<ReactDOM.style> => {
    if (
        attrs->Belt_MapString.findFirstBy((attr,_) => styleAttrs->Array.includes(attr))->Belt.Option.isSome
        || addBorder
    ) {
        Some(ReactDOM.Style.make(
            ~fontWeight=?(attrs->Belt_MapString.get("font-weight")),
            ~fontFamily=?(attrs->Belt_MapString.get("font-family")),
            ~fontSize=?(attrs->Belt_MapString.get("font-size")),
            ~fontStyle=?(attrs->Belt_MapString.get("font-style")),
            ~color=?(attrs->Belt_MapString.get("color")),
            ~border=?(if (addBorder) {Some("1px solid black")} else {None}),
            ~borderCollapse=?(if (addBorder) {Some("collapse")} else {None}),
            ()
        ))
    } else {
        None
    }
}

let createDomProps = (attrs:Belt_MapString.t<string>, ~addBorder:bool):option<ReactDOM.domProps> => {
    if (attrs->Belt_MapString.size != 0 || addBorder) {
        Some({
            style:?createStyle(attrs, ~addBorder),
            href:?(attrs->Belt_MapString.get("href")),
            title:?(attrs->Belt_MapString.get("href")),
            target:?(attrs->Belt_MapString.get("href")->Belt_Option.map(_ => "_blank")),
        })
    } else {
        None
    }
}

let xmlToReactElem = (xml:Xml_parser.xmlNode):result<reElem,string> => {
    let saveChild = (childrenStack:Belt_MutableStack.t<(string,array<reElem>)>, child:reElem):unit => {
        switch childrenStack->Belt_MutableStack.top {
            | None => Exn.raiseError("childrenStack->Belt_MutableStack.top is None")
            | Some((_,children)) => children->Array.push(child)
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
                | Node({name}) => childrenStack->Belt_MutableStack.push((name,[]))
                | Text(_) => ()
            }
            None
        },
        ~postProcess = (childrenStack,node) => {
            let parentTagName = childrenStack->Belt_MutableStack.top
                ->Belt.Option.map(((parentTagName,_)) => parentTagName)->Belt.Option.getWithDefault("")
            let thisElem = switch node {
                | Text(str) => {
                    if (parentTagName == "table" || parentTagName == "tbody" || parentTagName == "thead" || parentTagName == "tr") {
                        Ok(React.null)
                    } else {
                        Ok(str->React.string)
                    }
                }
                | Node({name, attrs}) => {
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
                                        | Some((_,children)) => children
                                    }
                                    Ok(ReactDOM.createDOMElementVariadic(
                                        name, 
                                        ~props=?createDomProps(
                                            attrs,
                                            ~addBorder=("table" == name || "tr" == name || "td" == name)
                                        ), 
                                        children
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
                        if (thisElem !== React.null) {
                            childrenStack->saveChild(thisElem)
                        }
                        None
                    }
                }
            }
        },
        ()
    )

    switch resOpt {
        | None => Exn.raiseError("Got None as a result of convertion from XML to React Elem.")
        | Some(res) => res
    }
}
