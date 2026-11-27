// Runs async tasks one at a time, in the order they were added.

type t = {
    // Resolves when the last added task is finished, successfully or not.
    mutable lastDone: promise<unit>,
}

let make = ():t => {lastDone: Promise.resolve()}

let run = (q:t, task:unit => promise<'a>):promise<'a> => {
    let result = q.lastDone->Promise.then(() => task())
    q.lastDone = result->Promise.thenResolve(_ => ())->Promise.catch(_ => Promise.resolve())
    result
}
