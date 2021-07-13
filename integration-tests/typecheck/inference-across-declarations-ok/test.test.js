const test = require('ava');
const {exec} = require('../../setup');

test(
	'typecheck: inference across declarations, a slightly more complicated example',
	exec,
	__dirname,
	['-m', 'src/Main.elm'],
	async (program, t) => {
		const snapshot = await program;
		await t.context.cliSnapshot(snapshot);
	}
);
