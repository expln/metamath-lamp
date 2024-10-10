import {processRequest} from "./MM_wrk_worker.res.js";

onmessage = e => {
    processRequest(e.data)
}