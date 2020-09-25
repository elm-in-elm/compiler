#!/usr/bin/env node

const fs = require('fs').promises;
const path = require('path');
const assert = require('assert');
const {Elm} = require('./elm.js');

const warningCommentLines = `

-- AUTO GENERATED TEST CASES
--
-- Do not edit below this line or your changes will be overwritten by
-- tests/parser-tests/update.js


`
	.trim()
	.split('\n');

const TEST_FILE_PATH = path.join(__dirname, '..', 'tests', 'ParserLexerTestCases.elm');
const SNIPPETS_DIR_PATH = path.join(__dirname, 'snippets');

async function main() {
	const testFile = await fs.readFile(TEST_FILE_PATH, 'utf-8');

	const epilogueStart = testFile.indexOf(warningCommentLines[0]);
	assert.notStrictEqual(epilogueStart, -1);

	const testFileStart = testFile.slice(0, epilogueStart).trim();

	const snippets = await Promise.all(
		(await fs.readdir(SNIPPETS_DIR_PATH)).sort().map(async (name) => ({
			name,
			source: await fs.readFile(path.join(SNIPPETS_DIR_PATH, name), 'utf-8'),
		}))
	);

	const tests = await new Promise((resolve) => {
		const app = Elm.Update.init({flags: snippets});
		app.ports.output.subscribe(resolve);
	});

	const newTestFile = [
		testFileStart,
		'\n\n',
		warningCommentLines.join('\n'),
		'\n',
		'testCases :',
		'    List',
		'        { contextualized : Maybe (List (Result ( State, Error ) Block))',
		'        , lexed : Result error (List (Located LexItem))',
		'        , name : String',
		'        , source : String',
		'        }',
		'testCases =',
		tests,
	].join('\n');

	await fs.writeFile(TEST_FILE_PATH, newTestFile);
}

main();
