const test = require('ava');
const {exec} = require('../../../setup');

test(
	'parser: indentation: valid custom types',
	exec,
	__dirname,
	['-m', 'src/Main.elm'],
	async (program, t) => {
		const snapshot = await program;
		await t.context.cliSnapshot(snapshot);
	}
);
