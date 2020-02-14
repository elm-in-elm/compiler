const test = require('ava');
const {exec} = require('../../setup');

test(
    'parser error: missing equals sign',
    exec,
    __dirname,
    ["-m", "src/Main.elm"],
    async program => ({snapshot: await program})
);
