
@val external window: {..} = "window"
@val external document: {..} = "document"

let minMax = (value:int, min:int, max:int):int => {
    Js_math.min_int(max, Js_math.max_int(min, value))
}

let canvas = document["createElement"](. "canvas")

let getTextWidth = (text:string, font:string):int => {
    let context = canvas["getContext"](. "2d")
    context["font"] = font
    let metrics = context["measureText"](. text)
    metrics["width"]
}

let calcColumnWidth = (cellClassName:string,min:int,max:int):int => {
    let cells = document["getElementsByClassName"](. cellClassName)
    if (cells["length"] > 0) {
        let font = (window["getComputedStyle"](. cells["item"](. 0)))["getPropertyValue"](. "font")
        let maxWidth = ref(0)
        for i in 0 to cells["length"] - 1 {
            maxWidth := Js_math.max_int(maxWidth.contents, getTextWidth(cells["item"](. i)["innerText"], font))
        }
        minMax(maxWidth.contents,min,max)
    } else {
        min
    }
}