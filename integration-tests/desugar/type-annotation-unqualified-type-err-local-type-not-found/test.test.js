const test = require('ava');
const {exec} = require('../../setup');

test.failing(
    "desugar: local unqualified type in the type annotation is not found",
    exec,
    __dirname,
    ["-m", "src/Main.elm"],
    async program => ({snapshot: await program})
);
