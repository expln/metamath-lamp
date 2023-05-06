open MM_context
open MM_wrk_settings
open MM_react_common
open Expln_React_common
open Expln_React_Mui
open Expln_React_Modal

type props = {
    settingsV:int,
    settings:settings,
    preCtxV:int,
    preCtx:mmContext,
}

let propsAreSame = (a:props, b:props):bool => {
    a.settingsV == b.settingsV
    && a.preCtxV == b.preCtxV
}

let pageSize = 20
let nonDigitPattern = %re("/\D/g")

let make = React.memoCustomCompareProps(({
    settingsV,
    settings,
    preCtxV,
    preCtx,
}:props) => {
    let (labels, setLabels) = React.useState(() => [])
    let (pageIdx, setPageIdx) = React.useState(() => 0)

    let (goToPageText, setGoToPageText) = React.useState(() => "")

    let numOfPages = (labels->Js.Array2.length->Belt_Int.toFloat /. pageSize->Belt.Int.toFloat)
                        ->Js_math.ceil_float->Belt.Float.toInt
    let beginIdx = pageIdx*pageSize
    let endIdx = beginIdx + pageSize - 1

    let actChangePage = newPageNum => {
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

    React.useEffect2(() => {
        setLabels(_ => preCtx->getAllFrameLabels)
        None
    }, (settingsV, preCtxV))

    let rndPagination = () => {
        <Row alignItems=#center>
            <Pagination 
                count=numOfPages 
                page={pageIdx+1} 
                onChange={(_,newPage) => actChangePage(newPage)}
            />
            <TextField 
                size=#small
                style=ReactDOM.Style.make(~width="100px", ())
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
                    labels->Js_array2.slice(~start=beginIdx, ~end_=endIdx+1)->Js_array2.map(React.string)->React.array
                }
            </Col>
        }
    }

    rndFrames()

}, propsAreSame)