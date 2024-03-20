import * as Test_client from "/_build/default/test/client/test/test_client.js"

var elem = document.getElementById("content");
var app = undefined;
window.loadApp = (name) => {
  if (app) {
    console.log("Shutting down app");
    app.shutdown();
    elem.className = "app";
    app = undefined;
  }
  if (name) {
    console.log("Loading app:", name)
    elem.className = "app-" + name;
    app = Test_client[name](elem, 0);
  }
}
