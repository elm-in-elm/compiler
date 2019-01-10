// needs the Elm code to be compiled
const {Elm} = require('../build/elm.js');
const {registerPort} = require('./utils.js');
const fs = require('fs');
const fsPromises = fs.promises;

(async function(){

  const elmJson = await fsPromises.readFile('test/elm.json', {encoding: 'utf8'});

  const app = Elm.Main.init({
    flags: {
      mainFilePath: 'src/Main.elm',
      elmJson,
    }
  });

  registerPort(app, 'stdout', string => process.stdout.write(string));
  registerPort(app, 'stderr', string => process.stderr.write(string));
  registerPort(app, 'read', async function(filename) {
    const contents = await fsPromises.readFile(filename, {encoding: 'utf8'});
    app.ports.readSubscription.send([filename, contents]);
  });
  registerPort(app, 'write', async function({filePath,contents}) {
    await fsPromises.writeFile(filePath, contents);
  });

})();
