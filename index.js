// Cleaner URL, if user choose the default version, hide it in the url bar
document.querySelector("form").onsubmit = (e) => {
  const version = document.querySelector("#version").value;
  const latest = "27.1";
  if (version === latest) {
    e.preventDefault();
    const symbol = document.querySelector("#symbol").value;
    window.location.href = `?symbol=${encodeURIComponent(symbol)}`;
  }
};

if (window.location.search) {
  const output = document.querySelector("#output");
  const url = "/api/search" + window.location.search;
  output.textContent = `Loading ${url} ...`;
  fetch(url)
    .then((r) => r.json())
    .then((j) => {
      if ("error" in j) {
        output.textContent = `Error: ${j.error}`;
        return;
      }
      const { sym, doc } = j.data[0];
      output.innerHTML = `<h1>${sym}</h1><p>${doc}</p>`;
    })
    .catch((e) => {
      output.textContent = `Error: ${e}`;
    });
}
