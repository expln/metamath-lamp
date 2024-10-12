import './App.css';
import {make as MM_cmp_root} from "./metamath/ui/MM_cmp_root.res.js";
import {api} from "./metamath/ui/MM_cmp_api.res.js";

window.api = api

function App() {
  return <MM_cmp_root/>
}

export default App;
