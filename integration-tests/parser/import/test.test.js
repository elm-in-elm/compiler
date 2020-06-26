const test = require('ava');
const {exec} = require('../../setup');

test(
    'cli: able to have two source directories',
    exec,
    __dirname,
    ["-m", "src/Main.elm"],
    async program => ({snapshot: await program})
);
