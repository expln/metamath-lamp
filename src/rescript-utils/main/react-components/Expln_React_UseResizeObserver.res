@new external makeResizeObserver: (array<{..}> => unit) => {..} = "ResizeObserver"

let useClientSizeObserver = (ref:React.ref<Nullable.t<Dom.element>>, onClientSizeChange:(int,int)=>unit) => {
    React.useEffect1(() => {
        switch ref.current->Nullable.toOption {
            | Some(domElem) => {
                // Console.log2("domElem", domElem)
                let observer = makeResizeObserver(mutations => {
                    // Console.log2("mutations", mutations)
                    if (mutations->Array.length != 0) {
                        let target = (mutations->Array.getUnsafe(mutations->Array.length - 1))["target"]
                        let clientWidth = target["clientWidth"]
                        let clientHeight = target["clientHeight"]
                        // Console.log2("clientWidth", clientWidth)
                        // Console.log2("clientHeight", clientHeight)
                        onClientSizeChange(clientWidth, clientHeight)
                    }
                })
                observer["observe"](. domElem)->ignore
                Some(() => observer["disconnect"](.))
            }
            | None => None
        }
    }, [ref.current])
}