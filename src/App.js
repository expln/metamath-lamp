import './App.css';
import {make as MM_cmp_root} from "./metamath/ui/MM_cmp_root.bs";
import {fun, apiEntry} from "./metamath/ui/MM_cmp_api.bs";

window.fun = fun
window.api = apiEntry

function App() {
  return <MM_cmp_root/>
}

export default App;
