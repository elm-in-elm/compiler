const test = require('ava');
const {exec} = require('../../setup');

test(
    'parser: correctly parse if then else',
    exec,
    __dirname,
    ["-m", "src/Main.elm"],
    async program => ({snapshot: await program})
);
