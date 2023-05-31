type promise<'a> = Js.Promise.t<'a>
let promise: (('a => unit) => unit) => promise<'a>
let promiseFlatMap: (promise<'a>, 'a => promise<'b>) => promise<'b>
let promiseMap: (promise<'a>, 'a => 'b) => promise<'b>
