let promise = (procedure:('a => unit) => unit): promise<'a> => {
    Promise.make(
        (resolve, reject) => {
            try {
                procedure(result => resolve(result))
            } catch {
                | exn => {
                    reject(exn)
                }
            }
        }
    )
}
let promiseFlatMap = (promise, mapper) => promise -> Promise.then(mapper)
let promiseMap = (promise, mapper) => promise -> promiseFlatMap(value => Promise.resolve(mapper(value)))
let promiseResolved = (value:'a): promise<'a> => promise(resolve => resolve(value))