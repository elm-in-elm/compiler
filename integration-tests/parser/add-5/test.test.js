const test = require('ava');
const {exec} = require('../../setup');

test(
	'parser: a function that adds 5',
	exec,
	__dirname,
	['-m', 'src/Main.elm'],
	async (program, t) => {
		const snapshot = await program;
		await t.context.cliSnapshot(snapshot);
	}
);
