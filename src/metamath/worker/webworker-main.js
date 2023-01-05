import {processRequest} from "./MM_wrk_worker.bs";

onmessage = e => {
    processRequest(e.data)
}