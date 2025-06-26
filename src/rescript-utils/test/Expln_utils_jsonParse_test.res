let {log,log2} = module(Console)
let {describe,it,assertEq,fail} = module(Expln_test)

open Expln_utils_jsonParse

type param = {
    name: string,
    value: string,
}

describe("Expln_utils_json.parseObj", _ => {
    it("should parse simple object", _ => {
        //given
        let jsonStr = `{
            "name": "AAA",
            "value": "BBB",
            "num": 123.4,
            "numOpt": null,
            "int": 78,
            "boolFalse": false,
            "boolTrue": true,
            "boolOpt": null
        }`

        //when
        let p = parseJson(
            jsonStr, 
            asObj(_, d => {
                "name": d->str("name"),
                "value": d->str("value"),
                "num": d->num("num"),
                "numOpt": d->numOpt("numOpt"),
                "int": d->int("int"),
                "intOpt": d->intOpt("intOpt"),
                "boolFalse": d->bool("boolFalse"),
                "boolTrue": d->bool("boolTrue"),
                "boolOpt": d->boolOpt("boolOpt"),
            })
        )

        //then
        switch p {
            | Ok(param) =>
                assertEq(param["name"], "AAA")
                assertEq(param["value"], "BBB")
                assertEq(param["num"], 123.4)
                assertEq(param["numOpt"], None)
                assertEq(param["int"], 78)
                assertEq(param["intOpt"], None)
                assertEq(param["boolFalse"], false)
                assertEq(param["boolTrue"], true)
                assertEq(param["boolOpt"], None)
            | Error(msg) => {
                log2("Error: ", msg)
                fail()
            }
        }
    })
    it("returns a meaningful message when null is passed", _ => {
        //given
        let jsonStr = `null`

        //when
        let p = parseJson(
            jsonStr, 
            asObj(_, d => {
                name: d->str("name"),
                value: d->str("value"),
            })
        )

        //then
        switch p {
            | Error(msg) =>
                assertEq(msg, "Parse error: an object was expected at '/'.")
            | _ => fail()
        }
    })
    it("returns an error message when unparsable text is passed", _ => {
        //given
        let jsonStr = `null-`

        //when
        let p = parseJson(jsonStr, asObj(_, d => {
            name: d->str("name"),
            value: d->str("value"),
        }))

        //then
        switch p {
            | Error(msg) =>
                assertEq(msg, "Parse error: Unexpected non-whitespace character after JSON at position 4 (line 1 column 5)")
            | _ => fail()
        }
    })
    it("returns a meaningful message when null is passed for a non-null attribute", _ => {
        //given
        let jsonStr = `{
            "name": null,
            "value": "BBB"
        }`

        //when
        let p = parseJson(jsonStr, asObj(_, d => {
            name: d->str("name"),
            value: d->str("value"),
        }))

        //then
        switch p {
            | Error(msg) =>
                assertEq(msg, "Parse error: a string was expected at '/name'.")
            | _ => fail()
        }
    })
    it("returns a meaningful message when a non-null attribute is absent", _ => {
        //given
        let jsonStr = `{
            "name": "vvv"
        }`

        //when
        let p = parseJson(jsonStr, asObj(_, d => {
            name: d->str("name"),
            value: d->str("value"),
        }))

        //then
        switch p {
            | Error(msg) =>
                assertEq(msg, "Parse error: a string was expected at '/value'.")
            | _ => fail()
        }
    })
})

describe("Expln_utils_json.parseObjOpt", _ => {
    it("should return None when null is passed", _ => {
        //given
        let jsonStr = `null`

        //when
        let p = parseJson(jsonStr, asObjOpt(_, d => {
            name: d->str("name"),
            value: d->str("value"),
        }))

        //then
        switch p {
            | Ok(None) => ()
            | _ => fail()
        }
    })
})

describe("Expln_utils_json.asStrOpt", _ => {
    it("should return None when null is passed", _ => {
        //given
        let jsonStr = `{"arr":["A",null,"B"]}`

        //when
        let p = parseJson(jsonStr, asObj(_, d => {
            "arr": d->arr("arr", asStrOpt(_)),
        }))->Belt_Result.getExn

        //then
        assertEq(p, {"arr":[Some("A"),None,Some("B")]})
    })
})

describe("Expln_utils_json.asStr", _ => {
    it("should return an error when null is passed", _ => {
        //given
        let jsonStr = `{"arr":["A",null,"B"]}`

        //when
        let p = parseJson(jsonStr, asObj(_, d => {
            "arr": d->arr("arr", asStr(_)),
        }))

        //then
        assertEq(p, Error("Parse error: a string was expected at '/arr/1'."))
    })
})

describe("Expln_utils_json.asNumOpt", _ => {
    it("should return None when null is passed", _ => {
        //given
        let jsonStr = `{"arr":[23.8,null,41]}`

        //when
        let p = parseJson(jsonStr, asObj(_, d => {
            "arr": d->arr("arr", asNumOpt(_)),
        }))->Belt_Result.getExn

        //then
        assertEq(p, {"arr":[Some(23.8),None,Some(41.)]})
    })
})

describe("Expln_utils_json.asNum", _ => {
    it("should return an error when null is passed", _ => {
        //given
        let jsonStr = `{"arr":[23.8,null,41]}`

        //when
        let p = parseJson(jsonStr, asObj(_, d => {
            "arr": d->arr("arr", asNum(_)),
        }))

        //then
        assertEq(p, Error("Parse error: a number was expected at '/arr/1'."))
    })
})

describe("Expln_utils_json.asIntOpt", _ => {
    it("should return None when null is passed", _ => {
        //given
        let jsonStr = `{"arr":[23.8,null,41]}`

        //when
        let p = parseJson(jsonStr, asObj(_, d => {
            "arr": d->arr("arr", asIntOpt(_)),
        }))->Belt_Result.getExn

        //then
        assertEq(p, {"arr":[Some(23),None,Some(41)]})
    })
})

describe("Expln_utils_json.asInt", _ => {
    it("should return an error when null is passed", _ => {
        //given
        let jsonStr = `{"arr":[23.8,null,41]}`

        //when
        let p = parseJson(jsonStr, asObj(_, d => {
            "arr": d->arr("arr", asInt(_)),
        }))

        //then
        assertEq(p, Error("Parse error: an integer was expected at '/arr/1'."))
    })
})

describe("Expln_utils_json.asBoolOpt", _ => {
    it("should return None when null is passed", _ => {
        //given
        let jsonStr = `{"arr":[true,null,false]}`

        //when
        let p = parseJson(jsonStr, asObj(_, d => {
            "arr": d->arr("arr", asBoolOpt(_)),
        }))->Belt_Result.getExn

        //then
        assertEq(p, {"arr":[Some(true),None,Some(false)]})
    })
})

describe("Expln_utils_json.asBool", _ => {
    it("should return an error when null is passed", _ => {
        //given
        let jsonStr = `{"arr":[true,null,false]}`

        //when
        let p = parseJson(jsonStr, asObj(_, d => {
            "arr": d->arr("arr", asBool(_)),
        }))

        //then
        assertEq(p, Error("Parse error: a boolean was expected at '/arr/1'."))
    })
})

describe("Expln_utils_json.pathToStr", _ => {
    it("should return slash for empty path", _ => {
        assertEq(test_pathToStr(list{}), "/")
    })
    it("should return slash separated values for non-empty path", _ => {
        assertEq(test_pathToStr(list{"name", "14", "settings"}), "/settings/14/name")
    })
})