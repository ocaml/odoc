/* The browsers interpretation of the CORS origin policy prevents to run
   webworkers from javascript files fetched from the file:// protocol. This hack
   is to workaround this restriction. */
function createWebWorker() {
  var searchs = search_urls.map((search_url) => {
    let parts = document.location.href.split("/");
    parts[parts.length - 1] = search_url;
    return '"' + parts.join("/") + '"';
  });
  blobContents = ["importScripts(" + searchs.join(",") + ");"];
  var blob = new Blob(blobContents, { type: "application/javascript" });
  var blobUrl = URL.createObjectURL(blob);

  var worker = new Worker(blobUrl);
  URL.revokeObjectURL(blobUrl);

  return worker;
}

var worker;
var waiting = 0;

function wait() {
  waiting = waiting + 1;
  document.querySelector(".search-snake").classList.add("search-busy");
}

function stop_waiting() {
  if (waiting > 0) waiting = waiting - 1;
  else waiting = 0;
  if (waiting == 0) {
    document.querySelector(".search-snake").classList.remove("search-busy");
  }
}

document.querySelector(".search-bar").addEventListener("focus", (ev) => {
  if (typeof worker == "undefined") {
    worker = createWebWorker();
    worker.onmessage = (e) => {
      stop_waiting();
      let results = e.data;
      let search_results = document.querySelector(".search-result");
      search_results.innerHTML = "";
      let f = (entry) => {
        let search_result = document.createElement("a");
        search_result.classList.add("search-entry");
        search_result.href = base_url + entry.url;
        search_result.innerHTML = entry.html;
        search_results.appendChild(search_result);
      };
      results.forEach(f);
      let search_request = document.querySelector(".search-bar").value;
      if (results.length == 0 && search_request != "") {
        let no_result = document.createElement("div");
        no_result.classList.add("search-no-result");
        no_result.innerText = "No result...";
        search_results.appendChild(no_result);
      }
    };
  }
});

document.querySelector(".search-bar").addEventListener("input", (ev) => {
  wait();
  worker.postMessage(ev.target.value);
});


/** Navigation */

var n_focus = null;

let search_result_elt = document.querySelector(".search-result")

function search_results() {
  return search_result_elt.children;
}

function current_result() {
  return search_results()[n_focus];
}

function update_focus(new_focus) {
  if (n_focus !== null)
    current_result().classList.remove("search-nav-focus");
  n_focus = new_focus;
  if (new_focus !== null)
    current_result().classList.add("search-nav-focus");
}

function focus_previous_result() {
  if (n_focus === null)
    return;
  if (n_focus === 0)
    update_focus(null);
  else
    update_focus(n_focus - 1);
}

function focus_next_result() {
  if (n_focus === null)
    update_focus(0);
  else if (n_focus === search_results().length)
    update_focus(null);
  else
    update_focus(n_focus + 1);
}

function enter_focus() {
  if (n_focus === null) return;
  let elt = current_result();
  elt.click();
}

function enter_search() {
  document.querySelector(".search-bar").focus();
}

function is_searching() {
  return (document.querySelectorAll(".odoc-search:focus-within").length > 0);
}

function handle_key_down(event) {
  if (event.key === "/") {
    event.preventDefault();
    enter_search();
  }
  if (is_searching()) {
    if (event.key === "ArrowUp") {
      event.preventDefault();
      focus_previous_result();
    }
    if (event.key === "ArrowDown") {
      event.preventDefault();
      focus_next_result();
    }
    if (event.key === "Enter") {
      event.preventDefault();
      enter_focus();
    }
  }
}
document.addEventListener("keydown", handle_key_down);