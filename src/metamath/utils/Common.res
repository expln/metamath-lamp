let currTimeStr = () => Js.Date.now()->Js.Date.fromFloat->Js.Date.toISOString

type cache<'depVer,'dep,'data> = {
    recalc:'dep=>'data,
    depVerEq: ('depVer,'depVer) => bool,
    mutable depVer: option<'depVer>,
    mutable data: option<'data>,
}

let cacheMake = ( ~recalc:'dep=>'data, ~depVerEq: ('depVer,'depVer) => bool ) => {
    {recalc, depVerEq, depVer:None, data:None}
}

let cacheGetByDepVer = (cache, depVer) => {
    switch cache.depVer {
        | None => None
        | Some(cachedDepVer) => {
            if (cache.depVerEq(cachedDepVer,depVer)) {
                cache.data
            } else {
                None
            }
        }
    }
}

let cacheGet = (cache, depVer, dep) => {
    switch cacheGetByDepVer(cache, depVer) {
        | Some(data) => data
        | None => {
            cache.depVer = Some(depVer)
            let data = cache.recalc(dep)
            cache.data = Some(data)
            data
        }
    }
}