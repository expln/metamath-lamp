open Expln_test
open MM_wrk_pattern_search_v2

let frameMatchesPatternTest = (
    ~ctx:MM_context.mmContext, ~pattern:string, ~label:string
):matchResult => {
    let frm = ctx->MM_context.getFrameExn(label)
    switch parsePattern(pattern, ~ctx) {
        | Error(msg) => failMsg(`Failed to parse the pattern '${pattern}': ${msg}`)
        | Ok(patterns) => frameMatchesPatterns(frm, patterns)
    }
}

let assertFrameMatchesPatternWithIndices = (
    ~ctx:MM_context.mmContext, ~pattern:string, ~label:string, ~matchedIdxs:array<array<int>>
) => {
    assertEqMsg(
        frameMatchesPatternTest(~ctx, ~pattern, ~label),
        Matched(Some(matchedIdxs)),
        `Failed for pattern '${pattern}'`
    )
}

let assertFrameMatchesPattern = (
    ~ctx:MM_context.mmContext, ~pattern:string, ~label:string
) => {
    switch frameMatchesPatternTest(~ctx, ~pattern, ~label) {
        | Matched(_) => ()
        | _ => failMsg(`The pattern '${pattern}' didn't match the frame ${label}.`)
    }
}

let findFramesByPattern = (
    ~ctx:MM_context.mmContext, ~pattern:string
):array<string> => {
    let patterns = switch parsePattern(pattern, ~ctx) {
        | Error(msg) => failMsg(`Failed to parse the pattern '${pattern}': ${msg}`)
        | Ok(patterns) => patterns
    }
    let found = []
    ctx->MM_context.forEachFrame(frm => {
        switch frameMatchesPatterns(frm, patterns) {
            | Matched(_) => found->Array.push(frm.label)
            | _ => ()
        }
        None
    })->ignore
    found->Array.sort(String.compare)
    found
}

let mmFileText = Expln_utils_files.readStringFromFile("./src/metamath/test/resources/set-no-proofs._mm")
let (ast, _) = MM_parser.parseMmFile(~mmFileContent=mmFileText)
let ctx = MM_context.loadContext(ast)

describe("frameMatchesPatterns, simple scenarios", () => {
    it("matches the entire first hypothesis when the 's' flag is used", () => {
        assertFrameMatchesPatternWithIndices(~ctx, ~label="subrg1asclcl", 
            ~pattern="$s |- P = ( Poly1 ` R )",
            ~matchedIdxs=[[0,1,2,3,4,5,6,7],[],[],[],[],[],[],[],[]]
        )
    })
    it("matches the entire first hypothesis when the 'h' flag is used", () => {
        assertFrameMatchesPatternWithIndices(~ctx, ~label="subrg1asclcl", 
            ~pattern="$h |- P = ( Poly1 ` R )",
            ~matchedIdxs=[[0,1,2,3,4,5,6,7],[],[],[],[],[],[],[],[]]
        )
    })
    it("matches the entire first hypothesis when the 's' and 'h' flags are used", () => {
        assertFrameMatchesPatternWithIndices(~ctx, ~label="subrg1asclcl", 
            ~pattern="$hs |- P = ( Poly1 ` R )",
            ~matchedIdxs=[[0,1,2,3,4,5,6,7],[],[],[],[],[],[],[],[]]
        )
    })

    it("matches the entire middle hypothesis when the 's' flag is used", () => {
        assertFrameMatchesPatternWithIndices(~ctx, ~label="subrg1asclcl", 
            ~pattern="$s |- ( ph -> T e. ( SubRing ` R ) )",
            ~matchedIdxs=[[],[],[],[],[0,1,2,3,4,5,6,7,8,9,10,11],[],[],[],[]]
        )
    })
    it("matches the entire middle hypothesis when the 's' and 'h' flags are used", () => {
        assertFrameMatchesPatternWithIndices(~ctx, ~label="subrg1asclcl", 
            ~pattern="$hs |- ( ph -> T e. ( SubRing ` R ) )",
            ~matchedIdxs=[[],[],[],[],[0,1,2,3,4,5,6,7,8,9,10,11],[],[],[],[]]
        )
    })

    it("matches the entire last hypothesis when the 's' flag is used", () => {
        assertFrameMatchesPatternWithIndices(~ctx, ~label="gsumply1subr", 
            ~pattern="$s |- ( ph -> F : A --> B )",
            ~matchedIdxs=[[],[],[],[],[],[],[0,1,2,3,4,5,6,7,8,9],[]]
        )
    })
    it("matches the entire last hypothesis when the 's' and 'h' flags are used", () => {
        assertFrameMatchesPatternWithIndices(~ctx, ~label="gsumply1subr", 
            ~pattern="$hs |- ( ph -> F : A --> B )",
            ~matchedIdxs=[[],[],[],[],[],[],[0,1,2,3,4,5,6,7,8,9],[]]
        )
    })

    it("matches the entire assertion when the 's' flag is used", () => {
        assertFrameMatchesPatternWithIndices(~ctx, ~label="gsumply1subr", 
            ~pattern="$s |- ( ph -> ( S gsum F ) = ( U gsum F ) )",
            ~matchedIdxs=[[],[],[],[],[],[],[],[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]]
        )
    })
    it("matches the entire assertion when the 'a' flag is used", () => {
        assertFrameMatchesPatternWithIndices(~ctx, ~label="gsumply1subr", 
            ~pattern="$a |- ( ph -> ( S gsum F ) = ( U gsum F ) )",
            ~matchedIdxs=[[],[],[],[],[],[],[],[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]]
        )
    })

    it("matches the entire assertion for a frame without hypotheses when the 's' flag is used", () => {
        assertFrameMatchesPatternWithIndices(~ctx, ~label="addcomsr", 
            ~pattern="$s |- ( A +R B ) = ( B +R A )",
            ~matchedIdxs=[[0,1,2,3,4,5,6,7,8,9,10,11]]
        )
    })
    it("matches the entire assertion for a frame without hypotheses when the 'a' flag is used", () => {
        assertFrameMatchesPatternWithIndices(~ctx, ~label="addcomsr", 
            ~pattern="$a |- ( A +R B ) = ( B +R A )",
            ~matchedIdxs=[[0,1,2,3,4,5,6,7,8,9,10,11]]
        )
    })
    it("matches the entire assertion for a frame without hypotheses without flags", () => {
        assertFrameMatchesPatternWithIndices(~ctx, ~label="addcomsr", 
            ~pattern="|- ( A +R B ) = ( B +R A )",
            ~matchedIdxs=[[0,1,2,3,4,5,6,7,8,9,10,11]]
        )
    })
    
    it("returns correct indices when the first operand of the 'one of' operator matches a middle hyp", () => {
        assertFrameMatchesPatternWithIndices(~ctx, ~label="isum1p", 
            ~pattern="k e. Z ) -> ( F ` k $| k e. Z ) <-> ( F ` k $| k e. Z ] -> ( F ` k",
            ~matchedIdxs=[[],[],[5,6,7,8,9,10,11,12,13],[],[],[]]
        )
    })
    it("returns correct indices when the middle operand of the 'one of' operator matches asrt", () => {
        assertFrameMatchesPatternWithIndices(~ctx, ~label="isum1p", 
            ~pattern="$s k e. Z ) <-> ( F ` k $| F ` M ) + sum_ $| k e. Z ] -> ( F ` k",
            ~matchedIdxs=[[],[],[],[],[],[7,13,14,15,16,17]]
        )
    })
    it("returns correct indices when the last operand of the 'one of' operator matches asrt", () => {
        assertFrameMatchesPatternWithIndices(~ctx, ~label="isum1p", 
            ~pattern="$s+ k e. Z ) <-> ( F ` k $| k e. Z ] -> ( F ` k $| sum_ k e. ( ZZ>=",
            ~matchedIdxs=[[],[],[],[],[],[17,18,19,20,21]]
        )
    })

    it("returns correct indices when the last operand of the 'one of' operator matches asrt (single symbol)", () => {
        assertFrameMatchesPatternWithIndices(~ctx, ~label="2eximi", 
            ~pattern="$s <-> $| E.",
            ~matchedIdxs=[[],[2]]
        )
    })

    it("non-adj, each sub-sequence has 'h' or 'a', ordered", () => {
        assertFrameMatchesPattern(~ctx, ~label="ss2iundv", 
            ~pattern="
                $[h |- ( ( ph /\\ x e. A ) -> Y e. C ) $]
                $* $[h |- ( ( ph /\\ x e. A /\\ y = Y ) -> D = G ) $]
                $* $[h |- ( ( ph /\\ x e. A ) -> B C_ G ) $]
                $* $[a |- ( ph -> U_ x e. A B C_ U_ y e. C D ) $]
            "
        )
    })
    it("adj, each sub-sequence has 'h' or 'a', ordered", () => {
        assertFrameMatchesPattern(~ctx, ~label="ss2iundv", 
            ~pattern="
                $+
                $[h |- ( ( ph /\\ x e. A ) -> Y e. C ) $]
                $* $[h |- ( ( ph /\\ x e. A /\\ y = Y ) -> D = G ) $]
                $* $[h |- ( ( ph /\\ x e. A ) -> B C_ G ) $]
                $* $[a |- ( ph -> U_ x e. A B C_ U_ y e. C D ) $]
            "
        )
    })
    it("non-adj, each sub-sequence has 'h' or 'a', unordered", () => {
        assertFrameMatchesPattern(~ctx, ~label="ss2iundv", 
            ~pattern="
                $[h |- ( ( ph /\\ x e. A ) -> B C_ G ) $]
                $/ $[h |- ( ( ph /\\ x e. A ) -> Y e. C ) $]
                $/ $[a |- ( ph -> U_ x e. A B C_ U_ y e. C D ) $]
                $/ $[h |- ( ( ph /\\ x e. A /\\ y = Y ) -> D = G ) $]
            "
        )
    })
    it("adj, each sub-sequence has 'h' or 'a', unordered", () => {
        assertFrameMatchesPattern(~ctx, ~label="ss2iundv", 
            ~pattern="
                $+
                $[h |- ( ( ph /\\ x e. A ) -> B C_ G ) $]
                $/ $[h |- ( ( ph /\\ x e. A ) -> Y e. C ) $]
                $/ $[a |- ( ph -> U_ x e. A B C_ U_ y e. C D ) $]
                $/ $[h |- ( ( ph /\\ x e. A /\\ y = Y ) -> D = G ) $]
            "
        )
    })
})

describe("frameMatchesPatterns, complex scenarios", () => {
    it("some complex scenarios", () => {
        assertFrameMatchesPattern(~ctx, ~label="fourierdlem103", 
            ~pattern="
                $[h
                    i e. ( 0 ..^ M ) ) -> 
                    $* 
                    R e. ( ( F |` ( ( V ` i ) (,) ( V 
                    $* 
                    ` ( i + 1 ) ) ) ) limCC ( V ` i ) ) )
                $]
                $*
                $[
                    / ( 2 x. ( sin ` ( s / 2 ) ) ) ) ) ) 
                    $/ 
                    |- K = ( s e. ( -u _pi [,] _pi 
                    $/ 
                    ) |-> if ( s = 0 , 1 , ( s
                $]
                $*
                $[
                    |- ( ph -> ps ~~> ( W / 2 ) )
                    $|
                    |- ( ph -> Z ~~> ( W / 2 ) )
                    $|
                    |- ( ph -> Z ~~> ( x / 2 ) )
                $]
            "
        )
        assertFrameMatchesPattern(~ctx, ~label="fsumxp", 
            ~pattern="
                $[
                    |- ( z = <. 
                    $*
                    j , k >. -> 
                    $*
                    D = C )
                $]
                $*
                $[
                    e. Fin )
                    $/
                    -> B 
                    $/
                    |- ( ph 
                $]
                $*
                $[
                    j e. x y sum_ k e. B C
                    $|
                    j e. A sum_ k e. B C
                    $|
                    j e. A sum_ k e. RR C
                $]
            "
        )
        assertFrameMatchesPattern(~ctx, ~label="fsumxp", 
            ~pattern="
                $[
                    j e. x y sum_ k e. B C
                    $|
                    j e. A sum_ k e. B C
                    $|
                    j e. A sum_ k e. RR C
                $]
                $/
                $[
                    e. Fin )
                    $/
                    -> B 
                    $/
                    |- ( ph 
                $]
                $/
                $[
                    |- ( z = <. 
                    $*
                    j , k >. -> 
                    $*
                    D = C )
                $]
            "
        )
        assertFrameMatchesPattern(~ctx, ~label="fsumxp", 
            ~pattern="
                $[
                    j e. x y sum_ k e. B C
                    $|
                    j sum_ k B C
                    $|
                    j e. A sum_ k e. RR C
                $]
                $/
                $[
                    e. )
                    $/
                    -> B 
                    $/
                    ( ph 
                $]
                $/
                $[
                    |- z <. 
                    $*
                    , k -> 
                    $*
                    D C )
                $]
            "
        )
        assertFrameMatchesPattern(~ctx, ~label="fsumxp", 
            ~pattern="
                $+
                $[
                    x y sum_ k e. B
                    $|
                    A sum_ k e. B
                    $|
                    A sum_ k e. RR
                $]
                $/
                $[
                    e. Fin
                    $/
                    -> B 
                    $/
                    ( ph 
                $]
                $/
                $[
                    ( z =
                    $*
                    k >.
                    $*
                    = C
                $]
            "
        )
        assertFrameMatchesPattern(~ctx, ~label="fsumxp", 
            ~pattern="
                $+
                $[a
                    x y sum_ k e. B
                    $|
                    A sum_ k e. B
                    $|
                    A sum_ k e. RR
                $]
                $/
                $[h
                    e. Fin
                    $/
                    -> B 
                    $/
                    ( ph 
                $]
                $/
                $[h
                    ( z =
                    $*
                    k >.
                    $*
                    = C
                $]
            "
        )
        let complexPattern1="
            y e. B ph /\\ E. x e. A E. x e. ~~>
            $|
            y e. B ph /\\ E. x <-> e. A E. x e.
            $|
            $[
                $[
                    $[
                        e. B 
                        $* 
                        $[ 
                            E. x 
                            $| 
                            ph /\\ 
                        $] 
                    $]
                    $/
                    ( A. x e. A A. y 
                    
                $]
                $*
                $[
                    E. y e. 
                    $/
                    B ps ) ->
                $]
                $*
                $[
                    ( ph /\\ ps )
                    $/
                    x e. A E. y e. B 
                $]
            $]
        "
        assertFrameMatchesPattern(~ctx, ~label="2r19.29", ~pattern=complexPattern1 )
        assertFrameMatchesPattern(~ctx, ~label="2r19.29", ~pattern="$s "++complexPattern1 )
        assertFrameMatchesPattern(~ctx, ~label="2r19.29", ~pattern="$a "++complexPattern1 )

        let complexPattern2="
            $[h
                $[
                    r We x ) /\\ _om ~~>
                    $|
                    r We x ) /\\ _om ~<_
                $]
                $/
                $[
                    <-> ( ( x C_ A 
                    $*
                    /\\ r C_ ( x X.
                $]
            $]
            $/
            $[h
                $[
                    G : ~P A -1-1-> U_ 
                    $*
                    n e. _om ( A ^m n
                $]
                $/
                $[
                    |- ( ph ph
                    $|
                    |- ( ph
                $]
            $]
            $/
            $[a
                $[
                    A ^m n ) \\ U_ n 
                    $*
                    e. _om ( x ^m
                $]
                $/
                $[
                    ps ) -> D e. ( U_ n e. e. e.
                    $|
                    ps ) -> D e. ( U_ n e.
                $]
            $]
        "
        assertFrameMatchesPattern(~ctx, ~label="pwfseqlem1", ~pattern=complexPattern2 )
    })

    it("finds multiple frames by a pattern", () => {
        let mmFileText = Expln_utils_files.readStringFromFile("./src/metamath/test/resources/set-no-proofs._mm")
        let (ast, _) = MM_parser.parseMmFile(~mmFileContent=mmFileText)
        let ctx = MM_context.loadContext(ast, ~stopBefore="mathbox")
        assertEqMsg(
            findFramesByPattern(
                ~ctx, ~pattern="
                    $+
                    $[
                        x y sum_ k e. B
                        $|
                        A sum_ k e. B
                        $|
                        A sum_ k e. RR
                    $]
                    $/
                    $[
                        e. Fin
                        $/
                        -> B 
                        $/
                        ( ph 
                    $]
                    $/
                    $[
                        ( z =
                        $*
                        k >.
                        $*
                        = C
                    $]
                "
            ),
            ["fsum2d","fsumxp"],
            "case 1"
        )
        assertEqMsg(
            findFramesByPattern(
                ~ctx, ~pattern="
                    y e. B ph /\\ E. x e. A E. x e. ~~>
                    $|
                    y e. B ph /\\ E. x <-> e. A E. x e.
                    $|
                    $[
                        $[
                            $[
                                e. B 
                                $* 
                                $[ 
                                    E. x 
                                    $| 
                                    ph /\\ 
                                $] 
                            $]
                            $/
                            ( A. x e. A A. y 
                            
                        $]
                        $*
                        $[
                            E. y e. 
                            $/
                            B ps ) ->
                        $]
                        $*
                        $[
                            ( ph /\\ ps )
                            $/
                            x e. A E. y e. B 
                        $]
                    $]
                "
            ),
            ["2r19.29","mertenslem1","mertenslem2","ntrivcvgmul","r19.29d2r"],
            "case 2"
        )
    })
})

// describe("temp test", () => {
//     it("temp test", () => {
//     })
// })