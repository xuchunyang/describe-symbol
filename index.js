const versionSelect = document.querySelector("#version");
const symbolInput = document.querySelector("#symbol");
const form = document.querySelector("form");
const output = document.querySelector("#output");

// Hide latest (default) version in URL
form.onsubmit = (e) => {
  const latest = "27.1";
  if (versionSelect.value === latest) {
    e.preventDefault();
    const symbol = symbolInput.value;
    window.location.href = `?symbol=${encodeURIComponent(symbol)}`;
  }
};


const syncSearchParams = () => {
  const urlParams = new URLSearchParams(window.location.search);

  const version = urlParams.get("version");
  if (version && version !== versionSelect.value) {
    versionSelect.value = version;
  }

  const symbol = urlParams.get("symbol");
  if (symbol) {
    symbolInput.value = symbol;
  }
};

if (window.location.search) {
  syncSearchParams();
  
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
