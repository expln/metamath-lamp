open Expln_React_common
open Expln_React_Mui
open MM_react_common


let nonDigitPattern = %re("/\D/g")

@react.component
let make = ( 
    ~numOfPages:int,
    ~pageIdx:int,
    ~siblingCount:option<int>=?,
    ~itemsPerPage:int=0,
    ~showGoToPage:bool=false,
    ~showItemsPerPage:bool=false,
    ~onPageIdxChange:int=>unit,
    ~onItemsPerPageChange:option<int=>unit>=?,
    ~itemPerPageText:string="",
) => {
    let (goToPageText, setGoToPageText) = React.useState(() => "")
    let (itemsPerPageText, setItemsPerPageText) = React.useState(() => None)

    let itemsPerPageTextEffective = itemsPerPageText->Belt_Option.getWithDefault(itemsPerPage->Belt_Int.toString)

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

    let actChangeItemsPerPage = () => {
        switch itemsPerPageTextEffective->Belt_Int.fromString {
            | None => ()
            | Some(newItemsPerPage) => {
                switch onItemsPerPageChange {
                    | None => ()
                    | Some(onItemsPerPageChange) => onItemsPerPageChange(newItemsPerPage)
                }
                setItemsPerPageText(_ => None)
            }
        }
    }

    <Row alignItems=#center>
        <Pagination
            count=numOfPages 
            page={pageIdx+1} 
            ?siblingCount
            onChange={(_,newPage) => actChangePage(newPage)}
        />
        {itemPerPageText->React.string}
        {
            if (showItemsPerPage) {
                <input
                    type_="text" 
                    size=3
                    value=itemsPerPageTextEffective
                    onChange=evt2str(newItemsPerPage => {
                        setItemsPerPageText(_ => Some(newItemsPerPage->String.replaceRegExp(nonDigitPattern, "")))
                    })
                    onKeyDown=kbrdHnd(~key=keyEnter, ~act=actChangeItemsPerPage)
                />
            } else {
                React.null
            }
        }
        {
            if (showGoToPage) {
                <TextField 
                    size=#small
                    style=ReactDOM.Style.make(~width="150px", ())
                    label="Go to page" 
                    value=goToPageText 
                    onChange=evt2str(newPage => setGoToPageText(_ => newPage->String.replaceRegExp(nonDigitPattern, "")))
                    onKeyDown=kbrdHnd(~key=keyEnter, ~act=actGoToPage)
                />
            } else {
                React.null
            }
        }
    </Row>
}
