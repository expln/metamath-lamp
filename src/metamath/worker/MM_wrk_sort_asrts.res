open MM_context

type sortBy = UsageCnt | AsrtLen | AsrtLabel | NumOfHyps
type sortDir = Asc | Dsc

type sortBys = array<(sortBy, sortDir)>

type sorting = {
    sortBys:sortBys,
    comparator: Expln_utils_common.comparator<MM_context.frame>
}

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

let sortByGetTitle = sortBy => {
    switch allSortByOptions->Array.find(((v,_,_)) => v == sortBy) {
        | Some((_,_,title)) => title
        | None => panic(`Cannot find the title for the sortBy '${sortBy->sortByToStr}'.`)
    }
}

let sortDirToStr = sortDir => {
    switch sortDir {
        | Asc => "ASC"
        | Dsc => "DESC"
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

let makeComparatorSingle = (sortBy:sortBy, sortDir:sortDir):Expln_utils_common.comparator<frame> => {
    open Expln_utils_common
    let cmp = switch sortBy {
        | UsageCnt => Expln_utils_common.comparatorByInt(frm => frm.usageCnt)
        | AsrtLen => Expln_utils_common.comparatorByInt(frm => frm.asrt->Array.length)
        | AsrtLabel => Expln_utils_common.comparatorByStr(frm => frm.label)
        | NumOfHyps => Expln_utils_common.comparatorByInt(frm => frm.hyps->Array.length - frm.varHyps->Array.length)
    }
    switch sortDir {
        | Asc => cmp
        | Dsc => cmp->cmpRev
    }
}

let makeComparator = (sortBys:array<(sortBy, sortDir)>):Expln_utils_common.comparator<frame> => {
    if sortBys->Array.length == 0 {
        (_,_)=>Ordering.equal
    } else {
        let cmps = sortBys->Array.map(((sortBy,sortDir)) => makeComparatorSingle(sortBy,sortDir))
        if cmps->Array.length == 1 {
            cmps->Array.getUnsafe(0)
        } else {
            cmps->Array.reduceWithIndex(
                cmps->Array.getUnsafe(0),
                (acc,cmp,idx) => if idx == 0 {acc} else {Expln_utils_common.comparatorAndThen(acc,cmp)}
            )
        }
    }
}
