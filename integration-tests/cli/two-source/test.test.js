const test = require('ava');
const {exec} = require('../../setup');

test(
	'cli: supports multiple source directories',
	exec,
	__dirname,
	['-m', 'src/Main.elm'],
	async (program, t) => {
		const snapshot = await program;
		await t.context.cliSnapshot(snapshot);
	}
);
