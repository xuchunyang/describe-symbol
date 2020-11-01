require("dotenv").config();
const harperive = require("harperive");
const fs = require("fs");

const jsonFile = process.argv[2];
const json = JSON.parse(fs.readFileSync(jsonFile, "utf-8"));
const emacsVersion = json["emacs-version"];
// Avoid . in the table name because of select * from dev.dog
const tableName = "emacs_" + emacsVersion.replace(/\./g, "_");
const records = json.data;

const DB_CONFIG = {
  harperHost: process.env.DB_HOST,
  username: process.env.DB_USER,
  password: process.env.DB_PASS,
  schema: process.env.SCHEMA,
};

const Client = harperive.Client;
const client = new Client(DB_CONFIG);

client.createTable(
  {
    table: tableName,
    hashAttribute: "id",
  },
  (err, res) => {
    if (err) {
      console.log("Error:", err);
      return;
    }
    console.log(res);
    client.insert(
      {
        table: tableName,
        records: records,
      },
      (err, res) => {
        console.log(err ? err : res);
      }
    );
  }
);
