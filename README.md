# Lite Assistant for [Metamath](https://us.metamath.org) Proofs

This is the repository for metamath-lamp (Lite Assistant for Metamath Proofs).
Metamath-lamp is a proof assistant for creating formal
mathematical proofs in the [Metamath system](https://us.metamath.org/).
Unlike most other Metamath proof systems
(such as mmj2 or original metamath-exe),
users can use this proof assistant without installing anything;
you can simply run it directly using your web browser.
It's written in JavaScript using the React user interface framework.
It's written in [ReScript](https://rescript-lang.org/)
(a robustly typed programming language that compiles to JavaScript) using the
[React user interface library](https://react.dev/) and
[Material UI Components](https://mui.com/material-ui/getting-started/overview/).
It's licensed under the [MIT license](./LICENSE.txt).

You can use this proof assistant *now* by going to the
**[Metamath-lamp web site](https://expln.github.io/lamp/latest/index.html)**.

[Here's a short video demo](https://drive.google.com/file/d/1IwdHLpQreZ_1CJFZJmptRJc2unO8aNh4/view?usp=sharing).

## Common actions

***How to run locally:***

1. Clone the source code

`git clone https://github.com/expln/metamath-lamp.git`

2. Navigate to the project's directory

`cd metamath-lamp`

3. Install npm dependencies

`npm install`

4. Compile ReScript code (local and from all npm dependencies) to JS code

`npm run compile-with-deps`

5. Assemble a worker script

`npx webpack --config webworker.webpack.config.js`

6. Copy the worker script from ./dist to ./public

Windows: `copy /B /Y ".\dist\webworker-main.js" ".\public"`

Mac: `cp ./dist/webworker-main.js ./public`

7. Finally, run the project locally

`npm run start`

***How to run all unit tests***

`npm run compile-test-all-unit`

***How to run all integration tests***

1. Update setMmPath constant in src/metamath/test/MM_int_test_utils.res to point out to set.mm file.
2. Run tests: 
`npm run compile-test-all-int`


***How to run a particular test***

1. In package.json, update 'compile-test-single' script replacing put_test_name_here with required test name. For example:

replace: `"compile-test-single": "rescript && mocha --timeout 100000 -g \"'put_test_name_here'\" src/metamath/test/**/*.js",`

with: `"compile-test-single": "rescript && mocha --timeout 100000 -g \"'finds proofs for simple wffs'\" src/metamath/test/**/*.js",`

2. Run tests:

`npm run compile-test-single`

***How to debug***

There is no standard way to debug ReScript code. But ReScript gets compiled to readable JS code. So it is possible to debug JS code. In order to debug any use case:
1. Create a test which reproduces required use case or modify existing test.
2. Compile the code `npm run compile`
3. Use your favorite IDE to place a break point in generated JS code and then run tests in debug mode.
