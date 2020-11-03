# Online Emacs Documentation

https://describe-symbol.vercel.app/

## About

Emacs 25.1 introduced <kbd>C-h o</kbd> (`describe-symbol`) which shows documentation for all kinds
of symbols, i.e., function/variable/face/cl-type, this project dumps all (about 10,000) Emacs's
builtin symbols' documentation using it and provides a web interface.

## Features

- Supports Emacs 25.1, 26.1 and 27.1
- Clickable links to source file, info manual etc, similar to Emacs

## Build

### Database

To build the database:

1. Run `make json EMACS=25.1` to dump documentation into a JSON file.
2. Run `node add-elisp-demos-to-json.js` to add elisp-demos to the JSON file.
3. Run `node json-to-harperdb.js` to upload the JSON file to HarperDB, a `.env` file like the
   following will be required

```conf
DB_HOST=https://cloud-1-xuchunyang.harperdbcloud.com
DB_USER=fake_user
DB_PASS=fake_pass
SCHEMA=describe_symbol
```

### Website

To build the website, run `vercel dev`.

## Alternative

http://doc.endlessparentheses.com/ provides documentation for Emacs [25.0.50.2](http://doc.endlessparentheses.com/Var/emacs-version.html).
