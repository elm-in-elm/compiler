const test = require('ava');
const {exec} = require('../../setup');

test(
	'typecheck: inference across declarations, mutual recursion should be handled OK',
	exec,
	__dirname,
	['-m', 'src/Main.elm'],
	async (program, t) => {
		const snapshot = await program;
		await t.context.cliSnapshot(snapshot);
	}
);
