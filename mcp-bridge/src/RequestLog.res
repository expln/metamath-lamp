open NodeApi

// Starts every entry: a request with its response, or a single message.
let entryDelimiter = String.repeat("=", 100)
// Starts the log of each run of the server, so several runs can be appended to one file.
let startDelimiter = String.repeat("#", 100)

type pendingRequest = {
    time: Date.t,
    method: string,
    // The formatted request, see formatRequest.
    text: string,
}

type t = {
    write: string => unit,
    now: unit => Date.t,
    // Requests which haven't got a response yet, by JSON.stringify of their ids.
    pending: Map.t<string, pendingRequest>,
}

let pad = (num:int, len:int):string => num->Int.toString->String.padStart(len, "0")

let formatTimestamp = (d:Date.t):string => {
    let date = `${d->Date.getFullYear->pad(4)}-${(d->Date.getMonth + 1)->pad(2)}-${d->Date.getDate->pad(2)}`
    let time = `${d->Date.getHours->pad(2)}:${d->Date.getMinutes->pad(2)}:${d->Date.getSeconds->pad(2)}`
    `${date} ${time}.${d->Date.getMilliseconds->pad(3)}`
}

let getField = (json:JSON.t, name:string):option<JSON.t> => {
    switch json {
        | Object(dict) => dict->Dict.get(name)
        | _ => None
    }
}

let getString = (json:JSON.t, name:string):option<string> => {
    switch json->getField(name) {
        | Some(String(str)) => Some(str)
        | _ => None
    }
}

// Request ids are numbers or strings, they are written without quotes.
let idToString = (id:JSON.t):string => {
    switch id {
        | String(str) => str
        | id => JSON.stringify(id)
    }
}

// Every line of the log starts with a new line symbol, instead of ending with it.
let line = (text:string):string => "\n" ++ text

/*
* Formats a multi-line text as is. One new line symbol at the end of the text is dropped,
* because the next line of the log starts with its own new line symbol.
* An empty text produces no lines.
* */
let textLines = (text:string):string => {
    if (text == "") {
        ""
    } else if (text->String.endsWith("\n")) {
        line(text->String.slice(~start=0, ~end=text->String.length - 1))
    } else {
        line(text)
    }
}

let jsonLine = (json:option<JSON.t>):string => {
    switch json {
        | None => ""
        | Some(json) => line(JSON.stringify(json))
    }
}

let durationMs = (~from:Date.t, ~to:Date.t):string => {
    (to->Date.getTime -. from->Date.getTime)->Float.toFixed(~digits=0)
}

let formatRequest = (~time:Date.t, ~msg:JSON.t, ~id:JSON.t, ~method:string):string => {
    let params = msg->getField("params")
    let (title, body) = if (method == "tools/call") {
        let toolName = params->Option.flatMap(getString(_, "name"))->Option.getOr("")
        let args = params->Option.flatMap(getField(_, "arguments"))->Option.getOr(JSON.Object(Dict.make()))
        (`${method} ${toolName}`, jsonLine(Some(args)))
    } else {
        (method, jsonLine(params))
    }
    line(`>>> ${formatTimestamp(time)} REQUEST #${idToString(id)} ${title}`) ++ body
}

// The text items of the content are written as is, other items as JSON.
let formatToolContent = (content:array<JSON.t>):string => {
    content->Array.mapWithIndex((item, i) => {
        let separator = i == 0 ? "" : line(`--- content item ${(i + 1)->Int.toString} ---`)
        let text = switch (item->getString("type"), item->getString("text")) {
            | (Some("text"), Some(text)) => textLines(text)
            | _ => jsonLine(Some(item))
        }
        separator ++ text
    })->Array.join("")
}

let formatResponse = (~req:pendingRequest, ~time:Date.t, ~msg:JSON.t, ~id:JSON.t):string => {
    let (status, body) = switch (msg->getField("error"), msg->getField("result")) {
        | (Some(error), _) => (" JSON-RPC ERROR", jsonLine(Some(error)))
        | (None, Some(result)) if req.method == "tools/call" => {
            switch result->getField("content") {
                | Some(Array(content)) => {
                    let isError = result->getField("isError") == Some(JSON.Boolean(true))
                    (isError ? " TOOL ERROR" : "", formatToolContent(content))
                }
                | _ => ("", jsonLine(Some(result)))
            }
        }
        | (None, result) => ("", jsonLine(result))
    }
    let duration = durationMs(~from=req.time, ~to=time)
    line(`<<< ${formatTimestamp(time)} RESPONSE #${idToString(id)} (${duration} ms)${status}`) ++ body
}

/*
* Formats a message which is not a part of a request-response pair: a notification,
* or a message which cannot be matched with a request or a response.
* arrow - '>>>' for messages from the agent, '<<<' for messages to the agent.
* */
let formatSingleMessage = (~arrow:string, ~time:Date.t, ~msg:JSON.t):string => {
    let ts = formatTimestamp(time)
    switch (msg->getString("method"), msg->getField("id")) {
        | (Some(method), None) => line(`${arrow} ${ts} NOTIFICATION ${method}`) ++ jsonLine(msg->getField("params"))
        | (Some(method), Some(id)) => {
            line(`${arrow} ${ts} REQUEST #${idToString(id)} ${method}`) ++ jsonLine(msg->getField("params"))
        }
        | (None, _) => line(`${arrow} ${ts} MESSAGE`) ++ jsonLine(Some(msg))
    }
}

let writeEntry = (t:t, text:string):unit => {
    t.write(line(entryDelimiter) ++ text)
}

type makeParams = {
    write: string => unit,
    now: unit => Date.t,
}

let make = ({write, now}:makeParams):t => {
    let t = {write, now, pending: Map.make()}
    t.write(line(startDelimiter) ++ line(`### ${formatTimestamp(now())} LOG STARTED`))
    t
}

let makeFileWriter = (path:string):(string => unit) => {
    let fd = Fs.openForAppendSync(path)
    let errorReported = ref(false)
    text => {
        try {
            Fs.appendFileSync(fd, text)
        } catch {
            | exn => {
                if (!errorReported.contents) {
                    errorReported := true
                    BrowserBridge.log(`Could not write to the log file ${path}: ${exnToMsg(exn)}`)
                }
            }
        }
    }
}

let messageReceived = (t:t, msg:JSON.t):unit => {
    let time = t.now()
    switch (msg->getString("method"), msg->getField("id")) {
        | (Some(method), Some(id)) => {
            t.pending->Map.set(JSON.stringify(id), {time, method, text: formatRequest(~time, ~msg, ~id, ~method)})
        }
        | _ => t->writeEntry(formatSingleMessage(~arrow=">>>", ~time, ~msg))
    }
}

let messageSent = (t:t, msg:JSON.t):unit => {
    let time = t.now()
    let req = switch (msg->getString("method"), msg->getField("id")) {
        | (None, Some(id)) => t.pending->Map.get(JSON.stringify(id))->Option.map(req => (id, req))
        | _ => None
    }
    switch req {
        | Some((id, req)) => {
            t.pending->Map.delete(JSON.stringify(id))->ignore
            t->writeEntry(req.text ++ formatResponse(~req, ~time, ~msg, ~id))
        }
        | None => t->writeEntry(formatSingleMessage(~arrow="<<<", ~time, ~msg))
    }
}

let stderrWritten = (t:t, text:string):unit => {
    t->writeEntry(line(`!!! ${formatTimestamp(t.now())} STDERR`) ++ textLines(text))
}

let uncaughtException = (t:t, origin:string, text:string):unit => {
    let title = origin == "unhandledRejection" ? "UNHANDLED REJECTION" : "UNCAUGHT EXCEPTION"
    t->writeEntry(line(`!!! ${formatTimestamp(t.now())} ${title}`) ++ textLines(text))
}

let writePending = (t:t):unit => {
    let time = t.now()
    t.pending->Map.values->Array.fromIterator->Array.forEach(req => {
        let duration = durationMs(~from=req.time, ~to=time)
        t->writeEntry(
            req.text ++ line(`<<< ${formatTimestamp(time)} NO RESPONSE (the server exited after ${duration} ms)`)
        )
    })
    t.pending->Map.clear
}
