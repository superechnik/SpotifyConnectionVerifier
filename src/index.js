import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';


var bytes = localStorage.getItem("bytes");
var flags = bytes ? bytes.split(",").map(i => parseInt(i)) : null;

const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: flags,
});

const _ = app.ports.genRandomBytes.subscribe(n => {
  const buffer = new Uint8Array(n);
  crypto.getRandomValues(buffer);
  const bytes = Array.from(buffer);
  localStorage.setItem("bytes", bytes);
  app.ports.randomBytes.send(bytes);
});


// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
