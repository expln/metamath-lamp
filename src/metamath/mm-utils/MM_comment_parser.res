
type commentPart = 
    | Text(string)
    | MathMode(string)
    | LabelMode(string)

let mathModeBegin = "`"->String.codePointAt(0)
let labelModeBegin = "~"->String.codePointAt(0)
let space1 = " "->String.codePointAt(0)
let space2 = "\t"->String.codePointAt(0)
let space3 = "\r"->String.codePointAt(0)
let space4 = "\n"->String.codePointAt(0)
let space5 = "\f"->String.codePointAt(0)

let splitCommentIntoParts = (text:string):array<commentPart> => {
    let charAtRef = (idx:ref<int>):option<int> => {
        text->String.codePointAt(idx.contents)
    }
    let incRef = (ref:ref<int>):unit => {
        ref := ref.contents + 1
    }
    let isWhitespace = (ch:option<int>):bool => {
        /* Metamath.pdf, page 213: Whitespace: (' ' | '\t' | '\r' | '\n' | '\f') */
        ch == space1 || ch == space2 || ch == space3 || ch == space4 || ch == space5
    }
    let isWhitespaceAtRef = (idx:ref<int>):bool => {
        charAtRef(idx)->isWhitespace
    }
    let textBetween = (idx1:int, idx2:int):string => {
        text->String.substring(~start=idx1, ~end=idx2)
    }
    let textBetweenRef = (idx1:ref<int>, idx2:ref<int>):string => {
        textBetween(idx1.contents, idx2.contents)
    }

    let res = []
    let prevIdx = ref(0)
    let idx = ref(0)
    let maxIdx = text->String.length-1
    let mathMode = ref(false)
    let labelMode = ref(false)
    while (idx.contents <= maxIdx) {
        let curChar = charAtRef(idx)
        incRef(idx)
        if (curChar == mathModeBegin && charAtRef(idx) == mathModeBegin) {
            /* Metamath.pdf, page 141:
             Two consecutive grave accents `` are treated as a single actual grave
                accent (both inside and outside of math mode) and will not cause the output
                processor to enter or exit math mode.
            */
            incRef(idx)
        } else {
            /* set.mm, line about 247:
                These tildes, tokens, math symbols and backticks should be surrounded by spaces.
            */
            if (mathMode.contents) {
                if (curChar == mathModeBegin && (isWhitespaceAtRef(idx) || maxIdx < idx.contents)) {
                    incRef(idx)
                    res->Array.push( MathMode(textBetweenRef(prevIdx, idx)) )
                    mathMode := false
                    prevIdx := idx.contents
                }
            } else {
                if (curChar == labelModeBegin && charAtRef(idx) == labelModeBegin) {
                    /* Metamath.pdf, page 141:
                        If a literal tilde is desired (outside of math mode) instead of label
                        mode, use two tildes in a row to represent it.
                     */
                    incRef(idx)
                } else {
                    if (labelMode.contents) {
                        if (isWhitespace(curChar)) {
                            res->Array.push( LabelMode(textBetweenRef(prevIdx, idx)) )
                            labelMode := false
                            prevIdx := idx.contents
                        }
                    } else if (curChar == mathModeBegin && isWhitespaceAtRef(idx)) {
                        res->Array.push( Text(textBetween(prevIdx.contents, idx.contents-1)) )
                        mathMode := true
                        prevIdx := idx.contents - 1
                        incRef(idx)
                    } else if (curChar == labelModeBegin && isWhitespaceAtRef(idx)) {
                        res->Array.push( Text(textBetween(prevIdx.contents, idx.contents-1)) )
                        labelMode := true
                        prevIdx := idx.contents - 1
                        incRef(idx)
                    }
                }
            }
        }
    }
    if (prevIdx.contents <= maxIdx) {
        if (mathMode.contents) {
            res->Array.push( MathMode(textBetweenRef(prevIdx, idx)) )
        } else if (labelMode.contents) {
            res->Array.push( LabelMode(textBetweenRef(prevIdx, idx)) )
        } else if (prevIdx.contents > 0) {
            res->Array.push( Text(textBetweenRef(prevIdx, idx)) )
        } else {
            res->Array.push( Text(text) )
        }
    }
    res
}