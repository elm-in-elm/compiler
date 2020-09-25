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

// Import_re = (r"import\s+((?:[.\w]+\.)?(\w+))\s+(?:as (\w+)\s+)?"
//              r"exposing\s+\((\w+(?:,\s+\w+)*)\)")

// def processFile(file):
//     globals = []

//     module_name = Path(file).stem

//     last_line_empty = False

//     with fileinput.input(file, inplace=True) as f:
//         for line in f:
//             if line.startswith(warning_comment_lines[0]):
//                 break
//             else:
//                 print(line, end='')
//                 last_line_empty = line.strip() == ''
//                 importMatch = re.search(import_re, line)

//                 if importMatch is not None:
//                     # Use alias if it is there, otherwise use last part of
//                     # import.
//                     moduleAlias = importMatch[3]
//                     if moduleAlias is None:
//                         moduleAlias = importMatch[2]

//                     vars = map(
//                         lambda defName: "__{}_{}".format(
//                             moduleAlias, defName.strip()),
//                         importMatch[4].split(","),
//                     )

//                     globals.append("/* global {} */".format(", ".join(vars)))

//     unused_var_config = '{{ "varsIgnorePattern": "_{}_.*" }}'.format(
//         module_name)

//     with open(file, "a") as f:
//         if not last_line_empty:
//             print(file=f)

//         print("\n".join(warning_comment_lines), file=f)
//         print(file=f)
//         print('/* eslint no-unused-vars: ["error", {}] */'.format(
//             unused_var_config),
//               file=f)
//         print(file=f)
//         print("\n".join(globals), file=f)

// def main():
//     if len(sys.argv) < 2:
//         print("generate-globals.py: error! At least one path or glob required",
//               file=sys.stderr)
//         exit(1)

//     if "-h" in sys.argv or "--help" in sys.argv:
//         print(HELP)
//         exit(0)

//     for provided_glob in sys.argv[1:]:
//         for path in glob.glob(provided_glob, recursive=True):
//             processFile(path)

// main()
