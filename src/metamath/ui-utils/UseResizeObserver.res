@new external makeResizeObserver: (array<{..}> => unit) => {..} = "ResizeObserver"

let useClientHeightObserver = (ref:React.ref<Js.Nullable.t<Dom.element>>, onClientHeightChange:int=>unit) => {
    React.useEffect1(() => {
        switch ref.current->Js.Nullable.toOption {
            | Some(domElem) => {
                // Js.Console.log2("domElem", domElem)
                let observer = makeResizeObserver(mutations => {
                    // Js.Console.log2("mutations", mutations)
                    if (mutations->Js.Array2.length != 0) {
                        let clientHeight = mutations[mutations->Js.Array2.length - 1]["target"]["clientHeight"]
                        // Js.Console.log2("clientHeight", clientHeight)
                        onClientHeightChange(clientHeight)
                    }
                })
                observer["observe"](. domElem)->ignore
                Some(() => observer["disconnect"](.))
            }
            | None => None
        }
    }, [ref.current])
}