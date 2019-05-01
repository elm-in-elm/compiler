const fs = require('fs').promises; // needs Node.JS v10+

const {Elm}          = require('../build/elm.js'); // build using Makefile... no Webpack around here!
const {registerPort} = require('./utils.js');

// Async/await is nice! (needs Node.JS v7.6+)
(async function(){

  console.log('---------------------------');
  console.log('-- STARTING THE COMPILER --');
  console.log('---------------------------');

  const exampleProjectPath = 'example-project';

  const app = Elm.Main.init({
    flags: {
      mainFilePath: 'src/Main.elm',
      elmJson: await fs.readFile(`${exampleProjectPath}/elm.json`, {encoding: 'utf8'}),
    }
  });

  registerPort(app, 'stdout', string => process.stdout.write(string));
  registerPort(app, 'stderr', string => {
    process.stderr.write('\n');
    process.stderr.write('\n---------------------------');
    process.stderr.write('\n-- COMPILER ERROR ---------');
    process.stderr.write('\n---------------------------');
    process.stderr.write(`\n${string}`);
  });
  registerPort(app, 'read', async function(filename) {
    try {
      const contents = await fs.readFile(`${exampleProjectPath}/${filename}`, {encoding: 'utf8'});
      app.ports.readSubscription.send([filename, contents]);
    } catch (e) {
      console.log('---------------------------');
      console.log('-- DEVELOPMENT ERROR ------');
      console.log('---------------------------');
      console.log(`\n${e.message}`);
      if (e.code != null) {
        app.ports.readErrorSubscription.send([filename, e.code]);
      }
    }
  });
  registerPort(app, 'write', async function({filePath,contents}) {
    console.log('---------------------------');
    console.log('-- WRITING TO FS ----------');
    console.log('---------------------------');
    console.log(contents);
    await fs.writeFile(`${exampleProjectPath}/${filePath}`, contents);
  });

})();
