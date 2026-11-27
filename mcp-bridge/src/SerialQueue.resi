// Runs async tasks one at a time, in the order they were added.

type t

let make: unit => t

/*
* Adds a task to the queue. The task starts only after all previously added tasks are finished.
* Resolves or rejects as the task does. A failed task doesn't stop the tasks added after it.
* */
let run: (t, unit => promise<'a>) => promise<'a>
