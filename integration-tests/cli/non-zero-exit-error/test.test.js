const test = require('ava');
const {exec} = require('../../setup');

test(
	'cli: exit with non-zero exit code on compile error',
	exec,
	__dirname,
	['-m', 'src/Main.elm'],
	async (program, t) => {
		const snapshot = await t.throwsAsync(program);
		t.is(snapshot.code, 1);
		await t.context.cliSnapshot(snapshot);
	}
);
