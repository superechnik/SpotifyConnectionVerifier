import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

const configs = 
{
  "apiUrl": process.env.ELM_APP_BASE_URI,
  "attic": process.env.ELM_APP_ATTIC,
  "kitchen": process.env.ELM_APP_KITCKEN,
  "library": process.env.ELM_APP_LIBRARY,
  "familyRoom": process.env.ELM_APP_FAMILYROOM,
  "clienId": process.env.ELM_APP_CLIENT_ID,
  "clientSecret": process.env.ELM_APP_CLIENT_SECRET,
};

var flags = JSON.stringify(configs);

Elm.Main.init({
  node: document.getElementById('root'),
  flags: flags
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
