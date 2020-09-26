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

function typeOfResultCases(shouldParse) {
	const resultArgs = shouldParse ? 'never Block' : '(State, Error) never';
	return `
    List
        { contextualized : Maybe (List (Result ${resultArgs} ))
        , lexed : Result Never (List (Located LexItem))
        , name : String
        , source : String
        }
`;
}

const TEST_FILE_PATH = path.join(__dirname, '..', 'tests', 'ParserLexerTestCases.elm');
const BASE_SNIPPETS_DIR_PATH = path.join(__dirname, 'snippets');
const SNIPPETS_DIR_PATH = Object.freeze({
	shouldParse: path.join(BASE_SNIPPETS_DIR_PATH, 'should-parse'),
	shouldNotParse: path.join(BASE_SNIPPETS_DIR_PATH, 'should-not-parse')
});

function getTestCase(snippets) {
	return new Promise(resolve => {
		const app = Elm.Update.init({flags: snippets});
		app.ports.output.subscribe(resolve);
	});
}

async function main() {
	const testFile = await fs.readFile(TEST_FILE_PATH, 'utf-8');

	const epilogueStart = testFile.indexOf(warningCommentLines[0]);
	assert.notStrictEqual(epilogueStart, -1);

	const testFileStart = testFile.slice(0, epilogueStart).trim();

	const snippets = await Promise.all(
		Object.entries(SNIPPETS_DIR_PATH).map(async ([category, dirPath]) => {
			let files;
			try {
				files = await fs.readdir(dirPath);
			} catch (error) {
				if (error.code === 'ENOENT') {
					return [category, []];
				}

				throw error;
			}

			return [
				category,
				await Promise.all(
					files
						.sort()
						.filter(name => !name.startsWith('_'))
						.map(async name => ({
							name,
							source: await fs.readFile(path.join(dirPath, name), 'utf-8')
						}))
				)
			];
		})
	);

	const testCases = await Promise.all(
		snippets.map(async ([category, snippets2]) => [category, await getTestCase(snippets2)])
	);

	if (testCases.some(tests => tests.includes('Panic'))) {
		console.error('ERROR: One or more test cases panicked!');
		process.exitCode = 1;
	}

	const newTestFile = [
		testFileStart,
		'\n\n',
		warningCommentLines.join('\n'),
		'\n',
		testCases
			.flatMap(([category, testCases]) => [
				`${category}TestCases :`,
				typeOfResultCases(category === 'shouldParse'),
				`${category}TestCases =`,
				testCases,
				'\n'
			])
			.join('\n')
	].join('\n');

	await fs.writeFile(TEST_FILE_PATH, newTestFile);
}

main();
