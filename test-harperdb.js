require("dotenv").config();
const harperive = require("harperive");
const fs = require("fs");

const DB_CONFIG = {
  harperHost: process.env.DB_HOST,
  username: process.env.DB_USER,
  password: process.env.DB_PASS,
  schema: process.env.SCHEMA,
};

const Client = harperive.Client;
const client = new Client(DB_CONFIG);

function cb(err, data) {
  if (err) console.log("Error:", err);
  else console.log("Data:", JSON.stringify(data, null, 2));
}

client.describeSchema(
  {
    schema: process.env.SCHEMA,
  },
  cb
);

client.query(
  `select * from describe_symbol.emacs_25_1 where sym = 'cdr'`,
  (err, data) => {
    if (err) {
      console.log("ERROR:", err);
      return;
    }
    console.log(data.data[0].sym);
    console.log(data.data[0].doc);
  }
);

client.searchByValue(
  {
    table: "emacs_27_1",
    searchAttribute: "sym",
    searchValue: "alist-get",
    attributes: ["*"],
  },
  (err, res) => {
    if (err) console.log(err);
    else console.log(res);
  }
);
