/* TODO Is `require()`ing dependencies where we need them better than
        all at the top? Measure! It makes the code look very unusual
        so there should be a good reason for keeping it.
*/

// Async/await is nice! This means we need Node.JS v7.6 at minimum though.
(async function(){

  const fs = require('fs');
  const fsPromises = fs.promises;

  // The following line needs the Elm code to be compiled! See `Makefile`.
  // No Webpack around here!
  const {Elm} = require('../build/elm.js');

  const exampleProjectPath = 'example-project';

  const app = Elm.Main.init({
    flags: {
      mainFilePath: 'src/Main.elm',
      elmJson: await fsPromises.readFile(`${exampleProjectPath}/elm.json`, {encoding: 'utf8'}),
    }
  });

  const {registerPort} = require('./utils.js');

  registerPort(app, 'stdout', string => process.stdout.write(string));
  registerPort(app, 'stderr', string => process.stderr.write(string));
  registerPort(app, 'read', async function(filename) {
    try {
      const contents = await fsPromises.readFile(`${exampleProjectPath}/${filename}`, {encoding: 'utf8'});
      app.ports.readSubscription.send([filename, contents]);
    } catch (e) {
      console.log({e});
      app.ports.readErrorSubscription.send([filename, e.code]);
    }
  });
  registerPort(app, 'write', async function({filePath,contents}) {
    await fsPromises.writeFile(`${exampleProjectPath}/${filePath}`, contents);
  });

})();
