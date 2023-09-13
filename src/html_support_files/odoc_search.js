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

document.querySelector(".search-bar").addEventListener("focus", (ev) => {
  if (typeof worker == "undefined") {
    worker = createWebWorker();
    worker.onmessage = (e) => {
      let results = e.data;
      let search_results = document.querySelector(".search-result-inner");
      search_results.innerHTML = "";
      let f = (entry) => {
        /* entry */
        let search_result = document.createElement("a");
        search_result.classList.add("search-entry");
        search_result.href = base_url + entry.url;
        search_result.innerHTML = entry.html;
        search_results.appendChild(search_result);
      };
      results.forEach(f);
    };
  }
});

document.querySelector(".search-bar").addEventListener("input", (ev) => {
  worker.postMessage(ev.target.value);
});
