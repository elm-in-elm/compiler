const test = require('ava');
const {exec} = require('../../setup');

test(
    'parser: a function that adds 5',
    exec,
    __dirname,
    ["-m", "src/Main.elm"],
    async program => ({snapshot: await program})
);
