const versionSelect = document.querySelector("#version");
const symbolInput = document.querySelector("#symbol");
const form = document.querySelector("form");
const output = document.querySelector("#output");

const latestVersion = "27.1";

// Hide latest (default) version in URL
const handleSubmit = (e) => {
  if (versionSelect.value === latestVersion) {
    e && e.preventDefault();
    const symbol = symbolInput.value;
    window.location.href = `?symbol=${encodeURIComponent(symbol)}`;
  }
};
form.onsubmit = handleSubmit;

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

const str2html = (string) => {
  const div = document.createElement("div");
  div.textContent = string;
  return div.innerHTML;
};

// (elisp) Pattern-Matching Conditional
// https://www.gnu.org/software/emacs/manual/html_node/elisp/Pattern_002dMatching-Conditional.html
//
// (elisp) Property Search
// https://www.gnu.org/software/emacs/manual/html_node/elisp/Property-Search.html
//
// (emacs)Parentheses
// https://www.gnu.org/software/emacs/manual/html_node/emacs/Parentheses.html
//
// (cl)Loop Facility
// https://www.gnu.org/software/emacs/manual/html_node/cl/Loop-Facility.html
//
// refer chunyang-Info-get-current-node-html
const node2link = (node) => {
  node = node.trim();
  const match = node.match(/^(?:\((.+)\))?\s*(.*)\s*$/);
  const manual = match[1] || "emacs";
  const section = match[2] || "Top";
  // refer chunyang-org-info-map-anchor-url
  const basename = [...section.replace(/\s+/g, " ")]
    .map((char) => {
      if (/[a-zA-Z0-9]/.test(char)) return char;
      if (char === " ") return "-";
      const codepoint = char.charCodeAt(0);
      return "_" + codepoint.toString(16).padStart(4, "0");
    })
    .join("");
  return `https://www.gnu.org/software/emacs/manual/html_node/${manual}/${basename}.html`;
};

const renderDoc = (sym, emacsVersion, data) => {
  for (const link of data.links) {
    link.beg--;
    link.end--;
  }
  if (data.delimiters === null) {
    data.delimiters = [];
  }
  data.delimiters = data.delimiters.map((pos) => {
    return {
      beg: pos - 1,
      end: pos,
      type: "delimiter",
    };
  });
  const markups = data.links
    .concat(data.delimiters)
    .sort((a, b) => a.beg - b.beg);
  const idxs = [
    0,
    ...markups
      .map(({ beg, end }) => [beg, end])
      .reduce((acc, elt) => acc.concat(elt)),
    data.doc.length,
  ];
  let html = "";
  for (let i = 0; i < idxs.length - 1; i++) {
    const beg = idxs[i];
    const end = idxs[i + 1];
    const str = data.doc.slice(beg, end);
    const found = markups.find((elt) => elt.beg === beg);
    if (found) {
      switch (found.type) {
        case "delimiter":
          html += "<hr>";
          break;
        case "news":
          {
            const { file, linum } = found.data;
            const href = `https://github.com/emacs-mirror/emacs/blob/emacs-${emacsVersion}/etc/${file}#L${linum}`;
            html += `<a href="${href}">${str}</a>`;
          }
          break;
        case "info":
          {
            const { node } = found.data;
            const href = node2link(node);
            html += `<a href="${href}">${str}</a>`;
          }
          break;
        case "function-def":
        case "variable-def":
        case "face-def":
        case "cl-type-def":
          {
            if ("data" in found) {
              const { file, linum } = found.data;
              const href = `https://github.com/emacs-mirror/emacs/blob/emacs-${emacsVersion}/${file}#L${linum}`;
              html += `<a href="${href}">${str}</a>`;
            } else {
              html += str; // work-around for #2
            }
          }
          break;
        default:
          html += `<a href="/?symbol=${encodeURIComponent(str)}">${str}</a>`;
          break;
      }
    } else {
      html += str2html(str);
    }
  }
  output.innerHTML = "";
  const para = document.createElement("p");
  para.classList.add("doc");
  para.innerHTML = html;
  output.appendChild(para);

  if ("demo" in data && data.demo !== null) {
    const pre = document.createElement("pre");
    pre.classList.add("demo");
    pre.textContent = data.demo;
    output.appendChild(pre);
  }
};

const search = (queryString) => {
  const urlParams = new URLSearchParams(queryString);
  const symbol = urlParams.get("symbol");
  document.title = `${symbol} - describe-symbol`;
  const url = "/api/search" + queryString;
  output.textContent = `Searching ${symbol} ...`;
  fetch(url)
    .then((r) => r.json())
    .then((j) => {
      if ("error" in j) {
        output.textContent = `Error: ${j.error}`;
        return;
      }
      const { sym, doc } = j.data[0];
      renderDoc(sym, j["emacs-version"], j.data[0]);
    })
    .catch((e) => {
      output.textContent = `Error: ${e}`;
    });
};

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
