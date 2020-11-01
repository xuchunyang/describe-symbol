const harperive = require("harperive");

const DB_URL = "https://cloud-1-xuchunyang.harperdbcloud.com";
const DB_USER = "describe_symbol_reader";
const DB_PASS = "12345";
const SCHEMA = "describe_symbol";

const DB_CONFIG = {
  harperHost: DB_URL,
  username: DB_USER,
  password: DB_PASS,
  schema: SCHEMA,
};

module.exports = (req, res) => {
  // TODO Add cache, cors headers
  if (!req.query.symbol) {
    res.status(400).json({ error: "Missing symbol argument: symbol" });
    return;
  }

  const versions = ["25.1", "26.1", "27.1"];
  const emacsVersion =
    req.query["emacs-version"] || req.query.version || "27.1";
  if (!versions.includes(emacsVersion)) {
    res.status(400).json({
      error: `Invalid version ${emacsVersion}, only supports ${versions.join(
        ", "
      )}`,
    });
    return;
  }

  const Client = harperive.Client;
  const client = new Client(DB_CONFIG);

  client.searchByValue(
    {
      table: `emacs_${emacsVersion.replace(/\./g, "_")}`,
      searchAttribute: "sym",
      searchValue: req.query.symbol,
      attributes: ["sym", "doc", "id"],
    },
    (err, data) => {
      if (err) {
        res.status(500).json(err);
        return;
      }
      if (data.data.length === 0) {
        res.status(404).json({
          error: `Can't find ${req.query.symbol} in Emacs ${emacsVersion}`,
        });
        return;
      }
      const r = data.data[0];
      r["emacs-version"] = emacsVersion;
      res.status(200).json(r);
    }
  );
};
