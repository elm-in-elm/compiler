const childProcess = require ('child_process');
const util = require ('util');
const path = require('path');
const fs = require('fs').promises; // needs Node.JS v10+

const cliPath = path.join(__dirname, '..', 'cli', 'index.js');
const execFile = util.promisify(childProcess.execFile);

module.exports = {
    runCompiler,
    exec
};

function runCompiler(cwd, args) {
	return execFile(process.execPath, [cliPath, ...args], {
        cwd,
    });
}

async function exec(t, cwd, args, func) {
    const {snapshot} = await func(runCompiler(cwd, args), t);

    if (snapshot !== undefined) {
        t.snapshot(`elm-in-elm ${args.join(' ')}`, {id: `Invocation`});
        t.snapshot(snapshot.stderr, {id: `Stderr`});
        t.snapshot(snapshot.stdout, {id: `Stdout`});
        let out;
        try {
            out = await fs.readFile(path.join(cwd, 'out.js'), 'utf-8');
        } catch (e) {
            if (e.code !== 'ENOENT') {
                throw e;
            }
        }
        if (out !== undefined) {
            t.snapshot(out, {id: `out.js`});
        }
    }
}

exec.title = (providedTitle, argString) =>
	`${
		providedTitle === undefined ? '' : `${providedTitle}:`
	} elm-in-elm ${argString}`.trim();
