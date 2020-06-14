const test = require('ava');
const {exec} = require('../../setup');

test.failing(
    "desugar: qualified type in the type annotation is found but doesn't agree with the declaration",
    exec,
    __dirname,
    ["-m", "src/Main.elm"],
    async program => ({snapshot: await program})
);
