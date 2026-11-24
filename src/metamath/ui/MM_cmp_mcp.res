open Expln_React_common
open Expln_React_Mui
open Local_storage_utils

let defaultPortStr = MM_mcp_client.defaultPort->Int.toString

let locStorKeyPort = "mcp-port"
let locStorKeyCmdOrigins = "mcp-cmd-origins"
let locStorKeyCmdFilesDirs = "mcp-cmd-files-dirs"
let locStorKeyCmdServerJsPath = "mcp-cmd-server-js-path"

let parsePort = (str:string):option<int> => {
    let str = str->String.trim
    if (%re("/^\d+$/")->RegExp.test(str)) {
        str->Int.fromString->Option.flatMap(port => 1 <= port && port <= 65535 ? Some(port) : None)
    } else {
        None
    }
}

let statusToStr = (status:MM_mcp_client.status):string => {
    let port = MM_mcp_client.getPort()->Int.toString
    switch status {
        | Disconnected => "Disconnected."
        | Connecting => `Connecting to ${MM_mcp_client.serverHostUrl}:${port} ...`
        | WaitingForServer => `Waiting for the MCP server to start on port ${port} ...`
        | WaitingForOtherTab(reason) => `Waiting: ${reason}`
        | Connected => `Connected to ${MM_mcp_client.serverHostUrl}:${port}`
        | Failed(reason) => `Disconnected: ${reason}`
    }
}

let statusColor = (status:MM_mcp_client.status):string => {
    switch status {
        | Connected => "green"
        | Failed(_) => "red"
        | Disconnected => "black"
        | Connecting | WaitingForServer | WaitingForOtherTab(_) => "darkorange"
    }
}

let quoteShellArg = (arg:string):string => {
    if (arg != "" && %re("/^[A-Za-z0-9_\-./:,=@+%~]+$/")->RegExp.test(arg)) {
        arg
    } else {
        "'" ++ arg->String.replaceAll("'", `'"'"'`) ++ "'"
    }
}

let buildCommand = (~serverJsPath:string, ~port:string, ~origins:string, ~filesDirs:string):string => {
    let serverJsPath = serverJsPath->String.trim
    let parts = [
        "node",
        serverJsPath == "" ? "/absolute/path/to/metamath-lamp/mcp-bridge/server.js" : quoteShellArg(serverJsPath),
    ]
    let addArg = (name:string, value:string) => {
        let value = value->String.trim
        if (value != "") {
            parts->Array.push(name)
            parts->Array.push(quoteShellArg(value))
        }
    }
    parts->Array.push("--port")
    parts->Array.push(quoteShellArg(port->String.trim))
    addArg("--origins", origins)
    addArg("--files-dirs", filesDirs)
    parts->Array.join(" ")
}

@react.component
let make = () => {
    let (status, setStatus) = React.useState(() => MM_mcp_client.getStatus())
    let (portStr, setPortStr) = useStateFromLocalStorageStr(~key=locStorKeyPort, ~default=defaultPortStr)

    let (cmdOrigins, setCmdOrigins) = useStateFromLocalStorageStr(~key=locStorKeyCmdOrigins, ~default="")
    let (cmdFilesDirs, setCmdFilesDirs) = useStateFromLocalStorageStr(~key=locStorKeyCmdFilesDirs, ~default="")
    let (cmdServerJsPath, setCmdServerJsPath) = useStateFromLocalStorageStr(
        ~key=locStorKeyCmdServerJsPath, ~default=""
    )
    let (copiedToClipboard, setCopiedToClipboard) = React.useState(() => false)

    React.useEffect0(() => {
        setStatus(_ => MM_mcp_client.getStatus())
        let unsubscribe = MM_mcp_client.subscribe(status => setStatus(_ => status))
        Some(unsubscribe)
    })

    React.useEffect1(() => {
        if (copiedToClipboard) {
            let timerId = setTimeout(() => setCopiedToClipboard(_ => false), 2000)
            Some(() => clearTimeout(timerId))
        } else {
            None
        }
    }, [copiedToClipboard])

    let isEnabled = switch status {
        | Disconnected | Failed(_) => false
        | Connecting | WaitingForServer | WaitingForOtherTab(_) | Connected => true
    }
    let port = parsePort(portStr)

    let command = buildCommand(
        ~serverJsPath=cmdServerJsPath, ~port=portStr, ~origins=cmdOrigins, ~filesDirs=cmdFilesDirs
    )

    let actConnect = () => {
        switch port {
            | None => ()
            | Some(port) => MM_mcp_client.connect(~port)
        }
    }

    let actCopyCommand = () => {
        MM_react_common.copyToClipboard(command)->Promise.thenResolve(() => {
            setCopiedToClipboard(_ => true)
        })->ignore
    }

    let updateCmdParam = (setter:(string=>string)=>unit, newValue:string) => {
        setCopiedToClipboard(_ => false)
        setter(_ => newValue)
    }

    let rndConnection = () => {
        <Col spacing=1.>
            <span style={fontWeight:"bold"}> {"Connection"->React.string} </span>
            <Row alignItems=#center>
                <TextField
                    label="Port"
                    size=#small
                    style={width:"100px"}
                    value=portStr
                    disabled=isEnabled
                    error={port->Option.isNone}
                    onChange=evt2str(str => updateCmdParam(setPortStr, str))
                />
                {
                    if (isEnabled) {
                        <Button variant=#contained onClick={_=>MM_mcp_client.disconnect()}>
                            {"Disconnect"->React.string}
                        </Button>
                    } else {
                        <Button variant=#contained disabled={port->Option.isNone} onClick={_=>actConnect()}>
                            {"Connect"->React.string}
                        </Button>
                    }
                }
            </Row>
            <span style={color:statusColor(status)}> {statusToStr(status)->React.string} </span>
            {
                if (isEnabled) {
                    <span>
                        {React.string(
                            "This tab keeps reconnecting until \"Disconnect\" is clicked, "
                            ++ "so the MCP server may be started before or after connecting. "
                            ++ "Keep this browser tab visible (e.g. in a separate window), "
                            ++ "because browsers slow down background tabs."
                        )}
                    </span>
                } else {
                    React.null
                }
            }
        </Col>
    }

    let rndCmdParam = (~label:string, ~value:string, ~setter:(string=>string)=>unit, ~width:string) => {
        <TextField
            label
            size=#small
            style={width:width}
            value
            onChange=evt2str(str => updateCmdParam(setter, str))
        />
    }

    let rndCommandBuilder = () => {
        <Col spacing=1.>
            <span style={fontWeight:"bold"}> {"Command to start the MCP server"->React.string} </span>
            {rndCmdParam(
                ~label="Absolute path to server.js", ~value=cmdServerJsPath, ~setter=setCmdServerJsPath,
                ~width="600px"
            )}
            {rndCmdParam(~label="--origins", ~value=cmdOrigins, ~setter=setCmdOrigins, ~width="600px")}
            {rndCmdParam(~label="--files-dirs", ~value=cmdFilesDirs, ~setter=setCmdFilesDirs, ~width="600px")}
            <span style={fontSize:"0.9em", color:"grey"}>
                {React.string(
                    "--port: the WebSocket port, 8765 by default. "
                    ++ "--origins: comma separated list of additional page origins allowed to connect. "
                    ++ "--files-dirs: directories from which local files may be loaded, "
                    ++ "separated by ':' (';' on Windows)."
                )}
            </span>
            <Row alignItems=#center>
                <Paper style={padding:"5px", fontFamily:"monospace", overflowWrap:"anywhere"}>
                    {command->React.string}
                </Paper>
                <Button title="Copy to the clipboard" onClick={_=>actCopyCommand()}>
                    <MM_Icons.ContentCopy/>
                </Button>
                {
                    if (copiedToClipboard) {
                        "Copied to the clipboard."->React.string
                    } else {
                        React.null
                    }
                }
            </Row>
        </Col>
    }

    <Col spacing=3. style={margin:"10px"}>
        {rndConnection()}
        {rndCommandBuilder()}
    </Col>
}
