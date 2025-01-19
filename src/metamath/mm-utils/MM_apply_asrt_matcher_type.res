open MM_substitution

type hypMatcherType = Label(string) | Idx(int)

type applyAsrtResultMatcher = {
    frm:frmSubsData,
    matchAsrt:bool,
    hypMatchers:array<hypMatcherType>,
}