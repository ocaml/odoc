
/* The browsers interpretation of the CORS origin policy prevents to run
   webworkers from javascript files fetched from the file:// protocol. This hack
   is to workaround this restriction. */
function createWebWorker() {
  var searchs = search_urls.map((search_url) => {
    let parts = document.location.href.split("/");
    parts[parts.length - 1] = search_url;
    return parts.join("/");
  });
  blobContents = ['importScripts("' + searchs.join(",") + '");'];
  var blob = new Blob(blobContents, { type: "application/javascript" });
  var blobUrl = URL.createObjectURL(blob);

  var worker = new Worker(blobUrl);
  URL.revokeObjectURL(blobUrl);

  return worker;
}

var worker = createWebWorker();

document.querySelector(".search-bar").addEventListener("input", (ev) => {
  worker.postMessage(ev.target.value);
});

worker.onmessage = (e) => {
  let results = e.data;
  let search_result = document.querySelector(".search-result");
  search_result.innerHTML = "";
  let f = (entry) => {
    let container = document.createElement("a");
    container.href = base_url + entry.url;
    container.innerHTML = entry.html;
    search_result.appendChild(container);
  };
  results.map(f);
};
