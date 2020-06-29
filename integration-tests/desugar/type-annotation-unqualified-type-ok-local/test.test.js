const test = require('ava');
const {exec} = require('../../setup');

test.failing(
    "desugar: unqualified type in the type annotation is found (defined in the same module)",
    exec,
    __dirname,
    ["-m", "src/Main.elm"],
    async program => ({snapshot: await program})
);
