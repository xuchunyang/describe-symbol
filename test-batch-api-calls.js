const fs = require("fs");
const https = require("https");

const jsonFile = process.argv[2];
const limit = Number(process.argv[3]);

if (!(jsonFile && limit)) {
  console.log("usage: program JSON LIMIT");
  process.exit(1);
}

const json = JSON.parse(fs.readFileSync(jsonFile, "utf-8"));
const usedSymbols = [];
const nextRandomSymbol = () => {
  const idx = Math.floor(Math.random() * json.count);
  const symbol = json.data[idx].sym;
  if (usedSymbols.includes(symbol)) {
    return nextRandomSymbol();
  } else {
    usedSymbols.push(symbol);
    return symbol;
  }
};

const myFetch = (symbol) => {
  console.log(symbol);
  const url = `https://describe-symbol.vercel.app/api/search?symbol=${encodeURIComponent(
    symbol
  )}`;
  https.get(url, (res) => {
    if (res.statusCode !== 200) {
      console.log("ERROR");
      console.log(res.statusCode);
      console.log(res.headers);
    }
    let body = "";
    res.on("data", (chunk) => (body += chunk));
    res.on("end", () => {
      console.log(JSON.parse(body));
      if (usedSymbols.length < limit) {
        myFetch(nextRandomSymbol());
      }
    });
  });
};

myFetch(nextRandomSymbol());
