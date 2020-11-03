const fs = require("fs");

// (elisp-demos--export-json-file "elisp-demos.json")
const demos = JSON.parse(fs.readFileSync("elisp-demos.json", "utf-8"));

const jsonfile = process.argv[2];
const json = JSON.parse(fs.readFileSync(jsonfile, "utf-8"));

for (const entry of json.data) {
  const { sym } = entry;
  if (sym in demos) {
    entry.demo = demos[sym];
  }
}

const outputFile = jsonfile + "-plus-elisp-demos.json";
fs.writeFileSync(outputFile, JSON.stringify(json, null, 2));
console.log(`Wrote to ${outputFile}`);
