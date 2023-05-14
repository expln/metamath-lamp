open MM_context
open MM_wrk_settings
open MM_react_common
open Expln_React_common
open Expln_React_Mui
open Expln_React_Modal
open MM_substitution
open MM_parenCounter

let pageSize = 20
let nonDigitPattern = %re("/\D/g")

type props = {
    modalRef:modalRef,
    typeColors:Belt_HashMapString.t<string>,
    editStmtsByLeftClick:bool,

    preCtx:mmContext,
    frms: Belt_MapString.t<frmSubsData>,
    parenCnt: parenCnt,
    syntaxTypes:array<int>,

    labels:array<(int,string)>,
}

let propsAreSame = (a:props, b:props):bool => {
    a.typeColors === b.typeColors
    && a.editStmtsByLeftClick === b.editStmtsByLeftClick
    && a.preCtx === b.preCtx
    && a.labels === b.labels
}

let make = React.memoCustomCompareProps(({
    modalRef,
    typeColors,
    editStmtsByLeftClick,
    preCtx,
    syntaxTypes,
    frms,
    parenCnt,
    labels,
}) => {
    let (pageIdx, setPageIdx) = React.useState(() => 0)
    let (goToPageText, setGoToPageText) = React.useState(() => "")

    let numOfPages = (labels->Js.Array2.length->Belt_Int.toFloat /. pageSize->Belt.Int.toFloat)
                        ->Js_math.ceil_float->Belt.Float.toInt
    let beginIdx = pageIdx * pageSize
    let endIdx = beginIdx + pageSize - 1

    let actReset = () => {
        setPageIdx(_ => 0)
        setGoToPageText(_ => "")
    }

    React.useEffect1(() => {
        actReset()
        None
    }, [labels])

    let actChangePage = (newPageNum:int) => {
        if (1 <= newPageNum && newPageNum <= numOfPages) {
            setPageIdx(_ => newPageNum-1)
            setGoToPageText(_ => "")
        }
    }

    let actGoToPage = () => {
        switch goToPageText->Belt_Int.fromString {
            | None => ()
            | Some(newPageNum) => actChangePage(newPageNum)
        }
    }

    let rndPagination = () => {
        <Row alignItems=#center>
            <Pagination 
                count=numOfPages 
                page={pageIdx+1} 
                onChange={(_,newPage) => actChangePage(newPage)}
            />
            <TextField 
                size=#small
                style=ReactDOM.Style.make(~width="150px", ())
                label="Go to page" 
                value=goToPageText 
                onChange=evt2str(newPage => setGoToPageText(_ => newPage->Js.String2.replaceByRe(nonDigitPattern, "")))
                onKeyDown=kbrdHnd(~onEnter=actGoToPage, ())
            />
        </Row>
    }

    let rndFrameSummary = (order,label) => {
        switch preCtx->getFrame(label) {
            | None => React.null
            | Some(frame) => {
                <MM_cmp_pe_frame_summary
                    key={`${order->Belt.Int.toString}-${label}`}
                    modalRef
                    preCtx
                    syntaxTypes
                    frms
                    parenCnt
                    frame
                    order
                    typeColors
                    editStmtsByLeftClick
                />
            }
        }
    }

    let rndFrames = () => {
        if (labels->Js.Array2.length == 0) {
            "No assertions loaded."->React.string
        } else {
            <Col>
                {rndPagination()}
                {
                    labels->Js_array2.slice(~start=beginIdx, ~end_=endIdx+1)
                        ->Js_array2.map(((order,label)) => rndFrameSummary(order,label))
                        ->React.array
                }
            </Col>
        }
    }

    rndFrames()
}, propsAreSame)