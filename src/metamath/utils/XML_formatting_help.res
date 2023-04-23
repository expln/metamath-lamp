open Expln_React_Mui

let exampleFormattedText =
`
<h1>This is Heading1</h1>
<h2>This is Heading2</h2>
<h3>This is Heading3</h3>
<h4>This is Heading4</h4>
<h5>This is Heading5</h5>
<h6>This is Heading6</h6>
This is a hyperlink -> <a font-weight="bold" color="green" href="https://us.metamath.org">Metamath</a>.
<hr/>
Text may be separated by horizontal lines.
<hr/>
Text may be <b>bold</b>, <i>italic</i>, <span font-family="monospace" font-size="1.25em">monospace</span>, it may have <span font-size="0.6em">different</span> <span font-size="1.5em">sizes</span>
and <span color="blue">different</span> <span color="red">colors</span>.

<p>Long chunks of text may be grouped into paragraphs.</p>

<ul>
    <li>This is an unordered list</li>
    <li>item2</li>
    <li>item3</li>
</ul>

<ol>
    <li>This is an ordered list</li>
    <li>item2</li>
    <li>item3</li>
</ol>

<table>
    <tbody>
        <tr><td>This</td><td>is</td></tr>
        <tr><td>a</td><td>table</td></tr>
    </tbody>
</table>

<p>Monospaced multiline text:</p>
<pre>--- 2p3e5 -----------------------------
1|       | 3cn      | |- 3 e. CC
2|       | 2cn      | |- 2 e. CC
3|       | 3p2e5    | |- ( 3 + 2 ) = 5
4| 1,2,3 | addcomli | |- ( 2 + 3 ) = 5
---------------------------------------</pre>
`

@react.component
let make = (
    ~onClose:unit=>unit,
) => {
    
    <Paper style=ReactDOM.Style.make( ~padding="10px", () ) >
        <Col spacing=1.>
            <Button onClick={_=>onClose()} variant=#contained > {React.string("Close")} </Button>
            <Paper style=ReactDOM.Style.make( ~padding="10px", () )>
                <Static_XML_to_HTML xmlStr=exampleFormattedText />
            </Paper>
            <Paper style=ReactDOM.Style.make( ~padding="10px", () )>
                <pre>{exampleFormattedText->React.string}</pre>
            </Paper>
            <Button onClick={_=>onClose()} variant=#contained > {React.string("Close")} </Button>
        </Col>
    </Paper>
}