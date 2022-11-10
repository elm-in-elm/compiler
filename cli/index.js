const fs = require('fs').promises; // Needs Node.JS v10+
const yargs = require('yargs');

const {Elm} = require('../build/elm.js'); // Build using Makefile... no Webpack around here!
const {registerPort} = require('./utils.js');

// Async/await is nice! (needs Node.JS v7.6+)
(async function () {
	// Process command line arguments
	const argv = yargs
		.option('main', {
			alias: 'm',
			description: 'The main Elm file',
			type: 'string',
			demandOption: true
		})
		.option('output', {
			alias: 'o',
			description: 'The format to emit: js | json | python',
			type: 'string',
			default: 'js'
		})
		.help()
		.alias('help', 'h')
		.argv;

	console.log('---------------------------');
	console.log('-- STARTING THE COMPILER --');
	console.log('---------------------------');

	const mainFileContents = await fs.readFile(argv.main, {encoding: 'utf8'});
	const elmJson = await fs.readFile('./elm.json', {encoding: 'utf8'});

	const app = Elm.Main.init({
		flags: {
			mainFilePath: argv.main,
			mainFileContents,
			elmJson,
			outputFormat: argv.output
		}
	});

	registerPort(app, 'stdout', string => process.stdout.write(string));
	registerPort(app, 'stderr', string => {
		process.stderr.write('\n---------------------------');
		process.stderr.write('\n-- COMPILER ERROR ---------');
		process.stderr.write('\n---------------------------');
		process.stderr.write(`\n${string}`);
	});
	registerPort(app, 'read', async ({moduleName, filePath}) => {
		try {
			const fileContents = await fs.readFile(filePath, {encoding: 'utf8'});
			app.ports.readSubscription.send({
				filePath,
				fileContents
			});
		} catch (error) {
			if (error.code !== null && error.code !== undefined) {
				app.ports.readErrorSubscription.send({
					moduleName,
					filePath,
					errorCode: error.code
				});
			} else {
        throw error;
      }
		}
	});
	registerPort(app, 'writeToFile', async ({filePath, fileContents}) => {
		setTimeout(() => {
			process.stdout.write('---------------------------\n');
			process.stdout.write('-- WRITING TO FS ----------\n');
			process.stdout.write('---------------------------\n');
			process.stdout.write(`${fileContents}\n`);
		}, 0);
		await fs.writeFile(filePath, fileContents);
	});
	registerPort(app, 'setExitCode', code => {
		process.exitCode = code;
	});
})();
