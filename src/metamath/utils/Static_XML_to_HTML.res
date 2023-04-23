open Expln_React_Mui

@react.component
let make = React.memo( ( ~xmlStr:string ) => {
    let rndErr = (errorMsg,xmlStr) => {
        <Col>
            {React.string("An error happened when rendering description:")}
            <pre style=ReactDOM.Style.make(~color="red", ())>{React.string(errorMsg)}</pre>
            {React.string("Showing raw content:")}
            <pre style=ReactDOM.Style.make(~overflowWrap="break-word", ~whiteSpace="pre-wrap", ())>{React.string(xmlStr)}</pre>
        </Col>
    }

    switch Xml_parser.parseStr("<div>" ++ xmlStr ++ "</div>") {
        | Error(msg) => rndErr(msg,xmlStr)
        | Ok(xml) => {
            switch Xml_to_React.xmlToReactElem(xml) {
                | Error(msg) => rndErr(msg,xmlStr)
                | Ok(reElem) => {
                    <div style=ReactDOM.Style.make(~margin="0px", ~padding="0px", ())>
                        reElem
                    </div>
                }
            }
        }
    }
} )
