const fs = require('fs').promises; // needs Node.JS v10+

const { Elm } = require('../build/elm.js'); // build using Makefile... no Webpack around here!
const { registerPort } = require('./utils.js');

// Async/await is nice! (needs Node.JS v7.6+)
(async function () {

  // process command line arguments
  const yargs = require('yargs')
  const argv = yargs
    .option('main', {
      alias: 'm',
      description: 'The main Elm file',
      type: 'string',
      demandOption: true
    })
    .option('output', {
      alias: 'o',
      description: 'The format to emit: JavaScript or JSON',
      type: 'string',
      default: 'JavaScript'
    })
    .help()
    .alias('help', 'h')
    .argv;

  console.log('---------------------------');
  console.log('-- STARTING THE COMPILER --');
  console.log('---------------------------');

  const app = Elm.Main.init({
    flags: {
      mainFilePath: argv.main,
      elmJson: await fs.readFile(`./elm.json`, { encoding: 'utf8' }),
      outputFormat: argv.output
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
  registerPort(app, 'read', async function (filename) {
    try {
      const contents = await fs.readFile(filename, { encoding: 'utf8' });
      app.ports.readSubscription.send({
        filePath: filename,
        fileContents: contents,
      });
    } catch (e) {
      console.log('---------------------------');
      console.log('-- DEVELOPMENT ERROR ------');
      console.log('---------------------------');
      console.log(`\n${e.message}`);
      if (e.code != null) {
        app.ports.readErrorSubscription.send({
          filePath: filename,
          errorCode: e.code,
        });
      }
    }
  });
  registerPort(app, 'writeToFile', async function ({ filePath, fileContents }) {
    console.log('---------------------------');
    console.log('-- WRITING TO FS ----------');
    console.log('---------------------------');
    console.log(fileContents);
    await fs.writeFile(`${filePath}`, fileContents);
  });

})();
