open Expln_test
open MM_wrk_pattern_search_v2

let assertFrameMatchesPattern = (
    ~ctx:MM_context.mmContext, ~pattern:string, ~label:string, ~matchedIdxs:array<array<int>>
) => {
    let frm = ctx->MM_context.getFrameExn(label)
    switch parsePattern(pattern, ~ctx) {
        | Error(msg) => failMsg(`Failed to parse the pattern '${pattern}': ${msg}`)
        | Ok(patterns) => {
            assertEqMsg(
                frameMatchesPatterns(frm, patterns),
                Matched(Some(matchedIdxs)),
                `Failed for pattern '${pattern}'`
            )
        }
    }
}

describe("frameMatchesPatterns _integration test_", () => {
    let mmFileText = Expln_utils_files.readStringFromFile("./src/metamath/test/resources/set-no-proofs._mm")
    let (ast, _) = MM_parser.parseMmFile(~mmFileContent=mmFileText)
    let ctx = MM_context.loadContext(ast)

    it("matches the entire first hypothesis when the 's' flag is used", () => {
        assertFrameMatchesPattern(~ctx, ~label="subrg1asclcl", 
            ~pattern="$s |- P = ( Poly1 ` R )",
            ~matchedIdxs=[[0,1,2,3,4,5,6,7],[],[],[],[],[],[],[],[]]
        )
    })
    it("matches the entire first hypothesis when the 'h' flag is used", () => {
        assertFrameMatchesPattern(~ctx, ~label="subrg1asclcl", 
            ~pattern="$h |- P = ( Poly1 ` R )",
            ~matchedIdxs=[[0,1,2,3,4,5,6,7],[],[],[],[],[],[],[],[]]
        )
    })
    it("matches the entire first hypothesis when the 's' and 'h' flags are used", () => {
        assertFrameMatchesPattern(~ctx, ~label="subrg1asclcl", 
            ~pattern="$hs |- P = ( Poly1 ` R )",
            ~matchedIdxs=[[0,1,2,3,4,5,6,7],[],[],[],[],[],[],[],[]]
        )
    })

    it("matches the entire middle hypothesis when the 's' flag is used", () => {
        assertFrameMatchesPattern(~ctx, ~label="subrg1asclcl", 
            ~pattern="$s |- ( ph -> T e. ( SubRing ` R ) )",
            ~matchedIdxs=[[],[],[],[],[0,1,2,3,4,5,6,7,8,9,10,11],[],[],[],[]]
        )
    })
    it("matches the entire middle hypothesis when the 's' and 'h' flags are used", () => {
        assertFrameMatchesPattern(~ctx, ~label="subrg1asclcl", 
            ~pattern="$hs |- ( ph -> T e. ( SubRing ` R ) )",
            ~matchedIdxs=[[],[],[],[],[0,1,2,3,4,5,6,7,8,9,10,11],[],[],[],[]]
        )
    })

    it("matches the entire last hypothesis when the 's' flag is used", () => {
        assertFrameMatchesPattern(~ctx, ~label="gsumply1subr", 
            ~pattern="$s |- ( ph -> F : A --> B )",
            ~matchedIdxs=[[],[],[],[],[],[],[0,1,2,3,4,5,6,7,8,9],[]]
        )
    })
    it("matches the entire last hypothesis when the 's' and 'h' flags are used", () => {
        assertFrameMatchesPattern(~ctx, ~label="gsumply1subr", 
            ~pattern="$hs |- ( ph -> F : A --> B )",
            ~matchedIdxs=[[],[],[],[],[],[],[0,1,2,3,4,5,6,7,8,9],[]]
        )
    })

    it("matches the entire assertion when the 's' flag is used", () => {
        assertFrameMatchesPattern(~ctx, ~label="gsumply1subr", 
            ~pattern="$s |- ( ph -> ( S gsum F ) = ( U gsum F ) )",
            ~matchedIdxs=[[],[],[],[],[],[],[],[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]]
        )
    })
    it("matches the entire assertion when the 'a' flag is used", () => {
        assertFrameMatchesPattern(~ctx, ~label="gsumply1subr", 
            ~pattern="$a |- ( ph -> ( S gsum F ) = ( U gsum F ) )",
            ~matchedIdxs=[[],[],[],[],[],[],[],[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]]
        )
    })
})