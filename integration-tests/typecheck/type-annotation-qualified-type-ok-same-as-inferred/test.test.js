const test = require('ava');
const {exec} = require('../../setup');

test(
	'typecheck: qualified type in the type annotation is found and agrees with the declaration',
	exec,
	__dirname,
	['-m', 'src/Main.elm'],
	async (program, t) => {
		const snapshot = await program;
		await t.context.cliSnapshot(snapshot);
	}
);
