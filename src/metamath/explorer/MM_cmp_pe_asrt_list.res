open MM_context
open MM_wrk_settings
open MM_react_common
open Expln_React_common
open Expln_React_Mui

let pageSize = 20
let nonDigitPattern = %re("/\D/g")

type props = {
    settingsVer:int,
    settings:settings,
    preCtxVer:int,
    preCtx:mmContext,
    labelsVer:int,
    labels:array<(int,string)>,
}

let propsAreSame = (a:props, b:props):bool => {
    a.settingsVer == b.settingsVer
    && a.preCtxVer == b.preCtxVer
    && a.labelsVer == b.labelsVer
}

let make = React.memoCustomCompareProps(({
    settingsVer,
    settings,
    preCtxVer,
    preCtx,
    labelsVer,
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
    }, [labelsVer])

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

    let rndFrames = () => {
        if (labels->Js.Array2.length == 0) {
            "No assertions loaded."->React.string
        } else {
            <Col>
                {rndPagination()}
                {
                    labels->Js_array2.slice(~start=beginIdx, ~end_=endIdx+1)
                        ->Js_array2.map(((order,label)) => `${order->Belt.Int.toString} - ${label}`->React.string)
                        ->React.array
                }
            </Col>
        }
    }

    rndFrames()
}, propsAreSame)