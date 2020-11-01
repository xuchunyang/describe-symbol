if (window.location.search) {
  const output = document.querySelector("#output");
  const url = "/api/search" + window.location.search;
  output.textContent = `Loading ${url} ...`;
  fetch(url)
    .then((r) => r.json())
    .then((j) => {
      if ("error" in j) {
        output("#output").textContent = `Error: ${j.error}`;
        return;
      }
      output.innerHTML = `<h1>${j.sym}</h1><p>${j.doc}</p>`;
    })
    .catch((e) => {
      output.textContent = `Error: ${e}`;
    });
}
