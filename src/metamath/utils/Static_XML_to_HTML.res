open Expln_React_common
open Expln_React_Mui
open MM_react_common

@react.component
let make = ( ~xmlStr:string ) => {
    let rndErr = (errorMsg,xmlStr) => {
        <Col>
            {React.string("An error happened when rendering description:")}
            <pre style=ReactDOM.Style.make(~color="red", ())>{React.string(errorMsg)}</pre>
            {React.string("Showing raw content:")}
            <pre>{React.string(xmlStr)}</pre>
        </Col>
    }

    switch Xml_parser.parseStr("<div>" ++ xmlStr ++ "</div>") {
        | Error(msg) => rndErr(msg,xmlStr)
        | Ok(xml) => {
            switch Xml_to_React.xmlToReactElem(xml) {
                | Error(msg) => rndErr(msg,xmlStr)
                | Ok(reElem) => reElem
            }
        }
    }
}