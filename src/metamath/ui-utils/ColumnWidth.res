
@val external window: {..} = "window"
@val external document: {..} = "document"

let minMax = (value:int, min:int, max:int):int => {
    Math.Int.min(max, Math.Int.max(min, value))
}

let canvas = document["createElement"](. "canvas")

let getTextWidth = (text:string, font:string):int => {
    let context = canvas["getContext"](. "2d")
    context["font"] = font
    let metrics = context["measureText"](. text)
    metrics["width"]
}

let calcColumnWidth = (query:string,min:int,max:int):int => {
    let cells = document["querySelectorAll"](. query)
    if (cells["length"] > 0) {
        let font = (window["getComputedStyle"](. cells["item"](. 0)))["getPropertyValue"](. "font")
        let maxWidth = ref(0)
        for i in 0 to cells["length"] - 1 {
            maxWidth := Math.Int.max(maxWidth.contents, getTextWidth(cells["item"](. i)["innerText"], font))
        }
        minMax(maxWidth.contents,min,max)
    } else {
        min
    }
}