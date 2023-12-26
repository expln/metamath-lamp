open Expln_React_common
open Expln_React_Mui
open MM_react_common


let nonDigitPattern = %re("/\D/g")

@react.component
let make = ( 
    ~numOfPages:int,
    ~pageIdx:int,
    ~siblingCount:option<int>=?,
    ~showGoToPage:bool=true,
    ~onPageIdxChange:int=>unit,
) => {
    let (goToPageText, setGoToPageText) = React.useState(() => "")

    let actChangePage = (newPageNum:int) => {
        if (1 <= newPageNum && newPageNum <= numOfPages) {
            setGoToPageText(_ => "")
            onPageIdxChange(newPageNum-1)
        }
    }

    let actGoToPage = () => {
        switch goToPageText->Belt_Int.fromString {
            | None => ()
            | Some(newPageNum) => actChangePage(newPageNum)
        }
    }

    <Row alignItems=#center>
        <Pagination
            count=numOfPages 
            page={pageIdx+1} 
            ?siblingCount
            onChange={(_,newPage) => actChangePage(newPage)}
        />
        {
            if (showGoToPage) {
                <TextField 
                    size=#small
                    style=ReactDOM.Style.make(~width="150px", ())
                    label="Go to page" 
                    value=goToPageText 
                    onChange=evt2str(newPage => setGoToPageText(_ => newPage->Js.String2.replaceByRe(nonDigitPattern, "")))
                    onKeyDown=kbrdHnd(~key=keyEnter, ~act=actGoToPage, ())
                />
            } else {
                React.null
            }
        }
    </Row>
}
