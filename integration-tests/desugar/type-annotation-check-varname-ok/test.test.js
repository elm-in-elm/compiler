const test = require('ava');
const {exec} = require('../../setup');

test(
	'desugar: type annotation var name agrees with the declaration var name',
	exec,
	__dirname,
	['-m', 'src/Main.elm'],
	async (program, t) => {
		const snapshot = await program;
		await t.context.cliSnapshot(snapshot);
	}
);
