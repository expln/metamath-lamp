open MM_context

type sortBy = UsageCnt | AsrtLen | AsrtLabel | NumOfHyps
type sortDir = Asc | Dsc

let panic = (msg:string):'a => Common.panic(`MM_wrk_sort_asrts: ${msg}`)

let sortByToStr = sortBy => {
    switch sortBy {
        | UsageCnt => "UsageCnt"
        | AsrtLen => "AsrtLen"
        | AsrtLabel => "AsrtLabel"
        | NumOfHyps => "NumOfHyps"
    }
}

let allSortByOptions = [
    (UsageCnt, sortByToStr(UsageCnt), "Usage count"),
    (AsrtLen, sortByToStr(AsrtLen), "Length of the assertion"),
    (AsrtLabel, sortByToStr(AsrtLabel), "Label"),
    (NumOfHyps, sortByToStr(NumOfHyps), "Number of hypotheses"),
]

let sortByFromStr = str => {
    switch allSortByOptions->Array.find(((_,c,_)) => c == str) {
        | Some((v,_,_)) => v
        | None => panic(`Cannot convert the value of '${str}' to a sortBy.`)
    }
}

let sortDirToStr = sortDir => {
    switch sortDir {
        | Asc => "Asc"
        | Dsc => "Dsc"
    }
}

let allSortDirOptions = [
    (Asc, sortDirToStr(Asc), "Ascending"),
    (Dsc, sortDirToStr(Dsc), "Descending"),
]

let sortDirFromStr = str => {
    switch allSortDirOptions->Array.find(((_,c,_)) => c == str) {
        | Some((v,_,_)) => v
        | None => panic(`Cannot convert the value of '${str}' to a sortDir.`)
    }
}

let makeComparator = (sortBy:sortBy, sortDir:sortDir):Expln_utils_common.comparator<frame> => {
    open Expln_utils_common
    let cmp = switch sortBy {
        | UsageCnt => panic("UsageCnt is not supported.")
        | AsrtLen => Expln_utils_common.comparatorByInt(frm => frm.asrt->Array.length)
        | AsrtLabel => Expln_utils_common.comparatorByStr(frm => frm.label)
        | NumOfHyps => panic("NumOfHyps is not supported.")
    }
    switch sortDir {
        | Asc => cmp
        | Dsc => cmp->cmpRev
    }
}