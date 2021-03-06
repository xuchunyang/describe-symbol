const DB_CONFIG = require("../db-config");
const harperive = require("harperive");

const Client = harperive.Client;
const client = new Client(DB_CONFIG);

module.exports = (req, res) => {
  res.setHeader("Access-Control-Allow-Origin", "*");

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
  const table = `${DB_CONFIG.schema}.emacs_${emacsVersion.replace(/\./g, "_")}`;

  client.query(`SELECT sym from ${table} order by sym`, (err, data) => {
    if (err) {
      res.status(500).json(err);
      return;
    }
    if (data.statusCode !== 200) {
      res.status(data.statusCode).json(data);
      return;
    }

    // FIXME work-around harperdb bug, see issue #1
    const symbols = data.data
      .map((x) => x.sym)
      .filter((sym) => sym !== null && sym !== undefined);
    console.log(symbols.length);
    const count = symbols.length;
    res.setHeader(
      "Cache-Control",
      `max-age=${3600 * 12}, s-maxage=${3600 * 24 * 30}`
    );
    res.status(200).json({
      count,
      symbols,
    });
  });
};
