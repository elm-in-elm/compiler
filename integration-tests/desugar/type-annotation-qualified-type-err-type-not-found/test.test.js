const test = require('ava');
const {exec} = require('../../setup');

test(
	'desugar: qualified type in the type annotation is not found in the imported module',
	exec,
	__dirname,
	['-m', 'src/Main.elm'],
	async (program, t) => {
		const snapshot = await t.throwsAsync(program);
		t.is(snapshot.code, 1);
		await t.context.cliSnapshot(snapshot);
	}
);
