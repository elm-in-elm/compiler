const test = require('ava');
const {exec} = require('../../setup');

test.failing(
    'desugar: type annotation var name agrees with the declaration var name',
    exec,
    __dirname,
    ["-m", "src/Main.elm"],
    async program => ({snapshot: await program})
);
