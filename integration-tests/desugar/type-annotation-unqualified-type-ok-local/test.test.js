const test = require('ava');
const {exec} = require('../../setup');

test(
	'desugar: unqualified type in the type annotation is found (defined in the same module)',
	exec,
	__dirname,
	['-m', 'src/Main.elm'],
	async (program, t) => {
		const snapshot = await program;
		await t.context.cliSnapshot(snapshot);
	}
);
