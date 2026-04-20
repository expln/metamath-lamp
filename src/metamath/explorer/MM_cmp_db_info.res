open MM_context
open Expln_React_Modal
open MM_wrk_pre_ctx_data
open Common
open Expln_React_Mui
open Expln_React_common

type props = {
    modalRef:modalRef,
    preCtxData:preCtxData,
}

let propsAreSame = (a:props, b:props):bool => {
    a.preCtxData === b.preCtxData
}

let make = React.memoCustomCompareProps(({
    modalRef,
    preCtxData,
}:props) => {
    let ctx = preCtxData.ctxV.val.full

    let monospaceFontStyle = ReactDOM.Style.make(
        ~fontFamily="monospace",
        ~fontSize="1.05em",
        ()
    )
    let baseTableStyle = ReactDOM.Style.make(
        ~borderCollapse="collapse", 
        ~border="1px solid black", 
        ~padding="5px",
        ~verticalAlign="verticalAlign",
        ()
    )
    let tdStyle = ReactDOM.Style.combine(baseTableStyle, monospaceFontStyle)
    let rowStyle = tdStyle
    let tableStyle = tdStyle

    let rndSection = (title: string, content: reElem):reElem => {
        <Accordion slotProps={{ "transition": { "timeout": 50 } }}>
            <AccordionSummaryStyled expandIcon={<MM_Icons.ExpandMore/>} >
                {title->React.string}
            </AccordionSummaryStyled>
            <AccordionDetails>
                content
            </AccordionDetails>
        </Accordion>
    }

    let rndConstants = () => {
        rndSection(
            "Constants",
            ctx->getAllConsts->Array.toSorted(String.compare)
                ->Array.map(const => 
                    <span key=const 
                        style=ReactDOM.Style.make(~marginRight="10px",~fontFamily="monospace",~fontSize="1.1em",())
                    >
                        {(const++" ")->React.string}
                    </span>
                )->React.array
        )
    }

    let rndVariables = () => {
        let typToVars = Belt_HashMapInt.make(~hintSize=10)
        ctx->getAllHyps->Belt_MapString.forEach((_,hyp) => {
            if (hyp.typ == F) {
                let typ = hyp.expr->Array.getUnsafe(0)
                let var = hyp.expr->Array.getUnsafe(1)
                switch typToVars->Belt_HashMapInt.get(typ) {
                    | None => typToVars->Belt_HashMapInt.set(typ, [var])
                    | Some(vars) => vars->Array.push(var)
                }
            }
        })
        let allVariables = typToVars->Belt_HashMapInt.toArray->Array.map(((typ,vars)) => {
            (
                ctx->ctxIntToSymExn(typ),
                vars->Array.map(ctxIntToSymExn(ctx, _))->Array.toSorted(String.compare)
                    ->Array.map(var => 
                        <span key=var style=ReactDOM.Style.make(~marginRight="10px",())>{(var++" ")->React.string}</span>
                    )->React.array
            )
        })->Array.toSorted(((a,_),(b,_)) => String.compare(a,b))
        rndSection(
            "Variables",
            <table style=tableStyle>
                <thead>
                    <tr style=rowStyle>
                        <th style=tdStyle>{"Type"->React.string}</th>
                        <th style=tdStyle>{"Variables"->React.string}</th>
                    </tr>
                </thead>
                <tbody>
                {
                    allVariables->Array.mapWithIndex(((typ,vars),idx) => {
                        <tr key={idx->Int.toString} style=rowStyle>
                            <td style=tdStyle>{typ->React.string}</td>
                            <td style=tdStyle>vars</td>
                        </tr>
                    })->React.array
                }
                </tbody>
            </table>
        )
    }

    let rndSyntaxTypes = () => {
        let defaultTypes = preCtxData.stmtTypeToSyntaxType->Belt_HashMapInt.get(0)
            ->Option.getOr([])
            ->Array.map(ctxIntToSymExn(ctx, _))->Array.toSorted(String.compare)
        let defaultTypesElem = defaultTypes->Array.map(typ => 
            <span key=typ 
                style=ReactDOM.Style.make(~marginLeft="10px",~fontFamily="monospace",~fontSize="1.15em",())
            >
                {(" "++typ)->React.string}
            </span>
        )->React.array
        let stmtTypeToSyntaxType = preCtxData.stmtTypeToSyntaxType->Belt_HashMapInt.toArray
            ->Array.filter(((typ,_)) => typ != 0)
            ->Array.map(((stmtType,syntaxTypes)) => {
                (
                    ctx->ctxIntToSymExn(stmtType),
                    syntaxTypes->Array.map(ctxIntToSymExn(ctx, _))->Array.toSorted(String.compare)
                        ->Array.map(syntaxType => (syntaxType++" ")->React.string)->React.array
                )
            })->Array.toSorted(((a,_),(b,_)) => String.compare(a,b))
        rndSection(
            "Syntax Types",
            <Col>
                {
                    if (stmtTypeToSyntaxType->Array.length > 0) {
                        <table style=tableStyle>
                            <thead>
                                <tr style=rowStyle>
                                    <th style=tdStyle>{"Statement type"->React.string}</th>
                                    <th style=tdStyle>{"Syntax types"->React.string}</th>
                                </tr>
                            </thead>
                            <tbody>
                            {
                                stmtTypeToSyntaxType->Array.mapWithIndex(((stmtType,syntaxTypes),idx) => {
                                    <tr key={idx->Int.toString} style=rowStyle>
                                        <td style=tdStyle>{stmtType->React.string}</td>
                                        <td style=tdStyle>syntaxTypes</td>
                                    </tr>
                                })->React.array
                            }
                            </tbody>
                        </table>
                    } else {
                        React.null
                    }
                }
                {
                    if (defaultTypes->Array.length > 0) {
                        <>
                            {"Default syntax types: "->React.string}
                            defaultTypesElem
                        </>
                    } else {
                        React.null
                    }
                }
            </Col>
        )
    }

    let rndAddComments = () => {
        rndSection(
            "Additional Information Comments",
            <pre> { ctx->getAddInfoComments->Array.join("\n\n")->React.string } </pre>
        )
    }

    <>
        {rndConstants()}
        {rndVariables()}
        {rndSyntaxTypes()}
        {rndAddComments()}
    </>
}, propsAreSame)