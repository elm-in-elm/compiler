const test = require('ava');
const {exec} = require('../../setup');

test(
    "desugar: unqualified type in the type annotation is found (imported)",
    exec,
    __dirname,
    ["-m", "src/Main.elm"],
    async program => ({snapshot: await program})
);
