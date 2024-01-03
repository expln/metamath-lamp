let promise = (procedure:('a => unit) => unit): promise<'a> => {
    Js.Promise.make(
        (~resolve, ~reject) => {
            try {
                procedure(result => resolve(. result))
            } catch {
                | exn => {
                    reject(. exn)
                }
            }
        }
    )
}
let promiseFlatMap = (promise, mapper) => promise -> Js.Promise.then_(mapper, _)
let promiseMap = (promise, mapper) => promise -> promiseFlatMap(value => Js_promise.resolve(mapper(value)))
let promiseResolved = (value:'a): promise<'a> => promise(resolve => resolve(value))