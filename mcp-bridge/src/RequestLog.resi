// Writes the MCP messages exchanged with the agent to a log, in a format meant to be read by a human.
//
// Each request is written together with its response, when the response is sent.
// Tool arguments, params and results which are JSON are written in a single line.
// Text content of tool results is written as is.
// See the example in README.md.

type t

type makeParams = {
    // Appends text to the log. Must not throw.
    write: string => unit,
    // Returns the current time.
    now: unit => Date.t,
}

/*
* Creates a log and writes the line which marks the start of the log.
* */
let make: makeParams => t

/*
* Returns a function which appends text to the file. The file is created if it doesn't exist.
* Throws if the file cannot be opened.
* Errors of later writes don't throw, the first of them is reported to stderr.
* */
let makeFileWriter: string => (string => unit)

/*
* Formats the time in the local time zone, e.g. 2026-11-27 14:05:12.345
* */
let formatTimestamp: Date.t => string

// Must be called for each message the agent sends.
let messageReceived: (t, JSON.t) => unit

// Must be called for each message sent to the agent.
let messageSent: (t, JSON.t) => unit

/*
* Writes all requests which haven't got a response yet, marking them as not answered.
* Meant to be called when the process exits.
* */
let writePending: t => unit
