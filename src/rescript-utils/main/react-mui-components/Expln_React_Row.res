open Expln_React_common

@react.component
let make = (
    ~gridRef:option<ReactDOM.domRef>=?,
    ~justifyContent:option<Expln_React_Grid.justifyContent>=?,
    ~alignItems:option<Expln_React_Grid.alignItems>=?,
    ~spacing:float=1.,
    ~style:option<reStyle>=?, 
    ~childXsOffset:option<int=>option<JSON.t>>=?,
    ~children:option<reElem>=?
) => {
    <Expln_React_Grid ref=?gridRef container=true direction=#row ?justifyContent ?alignItems spacing ?style >
        {switch children {
            | Some(children) => 
                children->React.Children.mapWithIndex((child,i) => {
                    let style = switch reElem2Obj(child)->Nullable.toOption {
                        | None => None
                        | Some(childObj) => {
                            switch childObj["props"]->Nullable.toOption {
                                | None => None
                                | Some(childProps) => {
                                    switch childProps["style"]->Nullable.toOption {
                                        | None => None
                                        | Some(childStyle) => {
                                            switch childStyle["display"]->Nullable.toOption {
                                                | None => None
                                                | Some(childDisplay) => {
                                                    if (childDisplay === "none") {
                                                        Some(ReactDOM.Style.make(~display="none", ()))
                                                    } else {
                                                        None
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                    <Expln_React_Grid 
                        ?style
                        xsOffset=?{ childXsOffset->Belt_Option.flatMap(childXsOffset => childXsOffset(i)) } 
                    >
                        child
                    </Expln_React_Grid>
                } )
            | None => React.null
        }}
    </Expln_React_Grid>
}