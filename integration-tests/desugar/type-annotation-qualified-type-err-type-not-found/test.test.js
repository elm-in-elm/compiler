const test = require('ava');
const {exec} = require('../../setup');

test.failing(
    "desugar: qualified type in the type annotation is not found in the imported module",
    exec,
    __dirname,
    ["-m", "src/Main.elm"],
    async program => ({snapshot: await program})
);
