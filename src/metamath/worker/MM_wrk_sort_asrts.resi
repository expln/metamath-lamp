type sortBy = UsageCnt | AsrtLen | AsrtLabel | NumOfHyps
type sortDir = Asc | Dsc

type sortBys = array<(sortBy, sortDir)>

type sorting = {
    sortBys:sortBys,
    comparator: Expln_utils_common.comparator<MM_context.frame>
}

let sortByToStr: sortBy => string
let sortByFromStr: string => sortBy
let sortByGetTitle: sortBy => string
let allSortByOptions: array<(sortBy, string, string)>
let sortDirToStr: sortDir => string
let sortDirFromStr: string => sortDir
let allSortDirOptions: array<(sortDir, string, string)>
let makeComparator: array<(sortBy, sortDir)> => Expln_utils_common.comparator<MM_context.frame>