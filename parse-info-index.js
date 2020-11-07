const fs = require("fs");
const https = require("https");
const cheerio = require("cheerio");

const getHtml = async (fileOrUrl) => {
  if (fileOrUrl.startsWith("https://")) {
    const t0 = new Date();
    return new Promise((resolve, reject) => {
      const req = https.get(fileOrUrl, (res) => {
        let body = "";
        res.on("data", (chunk) => (body += chunk));
        res.on("end", () => {
          const cost = new Date() - t0;
          console.log(`It takes ${cost / 1000}s to download ${fileOrUrl}`);
          resolve(body);
        });
      });
      req.on("error", (e) => {
        reject(e);
      });
    });
  } else {
    return fs.readFileSync(fileOrUrl, "utf-8");
  }
};

const getJson = async (fileOrUrl) => {
  const html = await getHtml(fileOrUrl);
  const $ = cheerio.load(html);
  const result = {};
  $("code", "a").each((i, elem) => {
    const symbol = $(elem).text();
    const href = $(elem.parent).attr("href");
    result[symbol] = href;
  });
  return result;
};

(async () => {
  // Emacs
  // https://www.gnu.org/software/emacs/manual/html_node/emacs/Command-Index.html
  // https://www.gnu.org/software/emacs/manual/html_node/emacs/Variable-Index.html
  // Emacs Lisp
  // https://www.gnu.org/software/emacs/manual/html_node/elisp/Index.html
  // CL-LIB
  // https://www.gnu.org/software/emacs/manual/html_node/cl/Function-Index.html
  // https://www.gnu.org/software/emacs/manual/html_node/cl/Variable-Index.html
  // Org
  // https://www.gnu.org/software/emacs/manual/html_node/org/Command-and-Function-Index.html
  // https://www.gnu.org/software/emacs/manual/html_node/org/Variable-Index.html
  // URL
  // https://www.gnu.org/software/emacs/manual/html_node/url/Function-Index.html
  // https://www.gnu.org/software/emacs/manual/html_node/url/Variable-Index.html
  // Widget
  // https://www.gnu.org/software/emacs/manual/html_node/widget/Index.html

  const indexes = [
    "https://www.gnu.org/software/emacs/manual/html_node/emacs/Command-Index.html",
    "https://www.gnu.org/software/emacs/manual/html_node/emacs/Variable-Index.html",
    "https://www.gnu.org/software/emacs/manual/html_node/elisp/Index.html",
    "https://www.gnu.org/software/emacs/manual/html_node/cl/Function-Index.html",
    "https://www.gnu.org/software/emacs/manual/html_node/cl/Variable-Index.html",
    "https://www.gnu.org/software/emacs/manual/html_node/org/Command-and-Function-Index.html",
    "https://www.gnu.org/software/emacs/manual/html_node/org/Variable-Index.html",
    "https://www.gnu.org/software/emacs/manual/html_node/url/Function-Index.html",
    "https://www.gnu.org/software/emacs/manual/html_node/url/Variable-Index.html",
    "https://www.gnu.org/software/emacs/manual/html_node/widget/Index.html",
  ];

  let result = {};
  for (let i = 0; i < indexes.length; i++) {
    const url = indexes[i];
    console.log(`${i}. Processing ${url} ...`);
    const json = await getJson(url);
    result = Object.assign(result, json);
    console.log(`Found ${Object.keys(json).length} symbols`);
    console.log(`${i}. Processing ${url} ...done`);
  }

  fs.writeFileSync("info-index.json", JSON.stringify(result, null, 2));
})();
