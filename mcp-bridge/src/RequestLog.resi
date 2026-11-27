// Writes the MCP messages exchanged with the agent to a log, in a format meant to be read by a human.
//
// Each message is written as soon as it is received or sent, so the log can be followed with tail -f.
// A response is written right below its request if nothing else was written in between,
// otherwise it starts a new entry; either way its header repeats the id and the title of the request.
// Tool arguments, params and results which are JSON are written in a single line.
// Text content of tool results and text written to stderr are written as is.
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
* Writes text which was written to stderr (by this server, a library, or Node.js) as a separate entry.
* */
let stderrWritten: (t, string) => unit

/*
* Writes an error which is about to crash the process, as a separate entry.
* origin - 'uncaughtException' or 'unhandledRejection', as passed to the 'uncaughtExceptionMonitor' event.
* text - the description of the error, e.g. its stack.
* */
let uncaughtException: (t, string, string) => unit

/*
* Writes all requests which haven't got a response yet, marking them as not answered.
* Meant to be called when the process exits.
* */
let writePending: t => unit
