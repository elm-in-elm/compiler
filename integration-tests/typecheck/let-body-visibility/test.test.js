const test = require('ava');
const {exec} = require('../../setup');

test.failing(
    'typecheck: the let body can see the let bindings',
    exec,
    __dirname,
    ["-m", "src/Main.elm"],
    async program => ({snapshot: await program})
);
