const test = require('ava');
const {exec} = require('../../setup');

test(
    'desugar: able to import variable from module',
    exec,
    __dirname,
    ["-m", "src/Main.elm"],
    async program => ({snapshot: await program})
);
