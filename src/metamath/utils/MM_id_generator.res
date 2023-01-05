
type idGenState = {
    nextId: int
}

let idGenMake = () => {
    {
        nextId: 0
    }
}

let idGenNext = st => {
    let newSt = {
        nextId: st.nextId+1
    }
    (newSt, st.nextId->Belt_Int.toString)
}