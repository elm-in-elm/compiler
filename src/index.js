const fs = require('fs').promises; // needs Node.JS v10+

const {Elm}          = require('../build/elm.js'); // build using Makefile... no Webpack around here!
const {registerPort} = require('./utils.js');

// Async/await is nice! (needs Node.JS v7.6+)
(async function(){

  const exampleProjectPath = 'example-project';

  const app = Elm.Main.init({
    flags: {
      mainFilePath: 'src/Main.elm',
      elmJson: await fs.readFile(`${exampleProjectPath}/elm.json`, {encoding: 'utf8'}),
    }
  });

  registerPort(app, 'stdout', string => process.stdout.write(string));
  registerPort(app, 'stderr', string => process.stderr.write(string));
  registerPort(app, 'read', async function(filename) {
    try {
      const contents = await fs.readFile(`${exampleProjectPath}/${filename}`, {encoding: 'utf8'});
      app.ports.readSubscription.send([filename, contents]);
    } catch (e) {
      console.log({e});
      app.ports.readErrorSubscription.send([filename, e.code]);
    }
  });
  registerPort(app, 'write', async function({filePath,contents}) {
    await fs.writeFile(`${exampleProjectPath}/${filePath}`, contents);
  });

})();
