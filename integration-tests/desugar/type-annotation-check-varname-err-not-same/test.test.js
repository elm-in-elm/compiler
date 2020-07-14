const test = require('ava');
const {exec} = require('../../setup');

test(
	'desugar: type annotation var name doesn\'t agree with the declaration var name',
	exec,
	__dirname,
	['-m', 'src/Main.elm'],
	async (program, t) => {
		const snapshot = await t.throwsAsync(program);
		t.is(snapshot.code, 1);
		await t.context.cliSnapshot(snapshot);
	}
);
