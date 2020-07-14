const childProcess = require('child_process');
const util = require('util');
const path = require('path');
const fs = require('fs').promises; // Needs Node.JS v10+

const cliPath = path.join(__dirname, '..', 'cli', 'index.js');
const execFile = util.promisify(childProcess.execFile);

module.exports = {
	runCompiler,
	exec
};

function runCompiler(cwd, args) {
	return execFile(process.execPath, [cliPath, ...args], {
		cwd
	});
}

async function exec(t, cwd, args, func) {
	try {
		await fs.unlink(path.join(cwd, 'out.js'));
	} catch (error) {
		if (error.code !== 'ENOENT') {
			throw error;
		}
	}

	t.context.cliSnapshot = snapshot => cliSnapshot(t, cwd, args, snapshot);
	const testOutput = await func(runCompiler(cwd, args), t);

	t.is(testOutput, undefined);
}

async function cliSnapshot(t, cwd, args, snapshot) {
	t.snapshot(`elm-in-elm ${args.join(' ')}`, {id: 'Invocation'});
	t.snapshot(snapshot.stderr, {id: 'Stderr'});
	t.snapshot(snapshot.stdout, {id: 'Stdout'});
	let out;
	try {
		out = await fs.readFile(path.join(cwd, 'out.js'), 'utf-8');
	} catch (error) {
		if (error.code !== 'ENOENT') {
			throw error;
		}
	}

	if (out !== undefined) {
		t.snapshot(out, {id: 'out.js'});
	}
}
