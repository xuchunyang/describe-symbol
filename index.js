const versionSelect = document.querySelector("#version");
const symbolInput = document.querySelector("#symbol");
const form = document.querySelector("form");
const output = document.querySelector("#output");

const latestVersion = "27.1";

// Hide latest (default) version in URL
form.onsubmit = (e) => {
  if (versionSelect.value === latestVersion) {
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

const search = (queryString) => {
  const url = "/api/search" + queryString;
  output.textContent = `Loading ${url} ...`;
  fetch(url)
    .then((r) => r.json())
    .then((j) => {
      if ("error" in j) {
        output.textContent = `Error: ${j.error}`;
        return;
      }
      const { sym, doc } = j.data[0];
      output.innerHTML = `<h1>${sym}</h1><pre>${doc}</pre>`;
    })
    .catch((e) => {
      output.textContent = `Error: ${e}`;
    });
}

if (window.location.search) {
  syncSearchParams();
  search(window.location.search);
} else {
  const urlParams = new URLSearchParams();
  if (versionSelect.value !== latestVersion) {
    urlParams.set("version", versionSelect.value);
  }
  urlParams.set("symbol", symbolInput.value);
  search("?" + urlParams);
}
