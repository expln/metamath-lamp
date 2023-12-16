import './App.css';
import {make as MM_cmp_root} from "./metamath/ui/MM_cmp_root.bs";
import {api} from "./metamath/ui/MM_cmp_editor.bs";

window.api = api

function App() {
  return <MM_cmp_root/>
}

export default App;
