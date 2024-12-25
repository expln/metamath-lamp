import './App.css';
import {make as MM_cmp_root} from "./metamath/ui/MM_cmp_root.res.js";
import {api} from "./metamath/api/MM_api.res.js";
import {api as api2} from "./metamath/api2/MM_api2.res.js";

window.api = api
window.api2 = api2

function App() {
  return <MM_cmp_root/>
}

export default App;
