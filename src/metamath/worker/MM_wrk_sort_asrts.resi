type sortBy = UsageCnt | AsrtLen | AsrtLabel | NumOfHyps
type sortDir = Asc | Dsc
let sortByToStr: sortBy => string
let sortByFromStr: string => sortBy
let allSortByOptions: array<(sortBy, string, string)>
let sortDirToStr: sortDir => string
let sortDirFromStr: string => sortDir
let allSortDirOptions: array<(sortDir, string, string)>
let makeComparator: (sortBy,sortDir) => Expln_utils_common.comparator<MM_context.frame>