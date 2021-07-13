const test = require('ava');
const {exec} = require('../../setup');

test(
	'typecheck: qualified type in the type annotation is found but doesn\'t agree with the declaration',
	exec,
	__dirname,
	['-m', 'src/Main.elm'],
	async (program, t) => {
		const snapshot = await t.throwsAsync(program);
		t.is(snapshot.code, 1);
		await t.context.cliSnapshot(snapshot);
	}
);
