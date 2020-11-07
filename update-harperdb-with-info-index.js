require("dotenv").config();
const harperive = require("harperive");
const fs = require("fs");

const indexFile = "info-index.json";
const json = JSON.parse(fs.readFileSync(indexFile, "utf-8"));

const DB_CONFIG = {
  harperHost: process.env.DB_HOST,
  username: process.env.DB_USER,
  password: process.env.DB_PASS,
  schema: process.env.SCHEMA,
};

const Client = harperive.Client;
const client = new Client(DB_CONFIG);

const updateTable = (emacsVersion) => {
  const table = `emacs_${emacsVersion.replace(/\./g, "_")}`;
  client.query(
    `SELECT id, sym from ${DB_CONFIG.schema}.${table}`,
    (err, data) => {
      if (err) {
        console.error(err);
        return;
      }
      const entries = data.data;
      console.log(`there are ${entries.length} rows in ${table}`);
      const updateRecords = [];
      for (const { id, sym } of entries) {
        if (sym in json) {
          const info = json[sym];
          updateRecords.push({ id, info });
        }
      }
      console.log(`To update ${updateRecords.length} records...`);
      client.update(
        {
          table,
          records: updateRecords,
        },
        (err, res) => {
          if (err) {
            console.error(err);
            return;
          }
          console.log("update table successfully");
          console.log(res);
        }
      );
    }
  );
};

// 25.1
// 26.1
// 27.1
updateTable(process.argv[2]);
