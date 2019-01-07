// needs the Elm code to be compiled
const {Elm} = require('../build/elm.js');
const {registerPort} = require('./utils.js');
const fs = require('fs');
const fsPromises = fs.promises;

const app = Elm.Main.init({
  flags: {
    mainFilepath: 'src/Main.elm'
  }
});

registerPort(app, 'stdout', string => process.stdout.write(string));
registerPort(app, 'stderr', string => process.stderr.write(string));
registerPort(app, 'readFile', async function(filename) {
  const contents = await fsPromises.readFile(filename, {encoding: 'utf8'});
  app.ports.readSubscription.send([filename, contents]);
});
registerPort(app, 'write', async function({filename,contents}) {
  await fsPromises.writeFile(filename, contents);
});
