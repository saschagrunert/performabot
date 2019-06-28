# performabot

[![CircleCI](https://circleci.com/gh/saschagrunert/performabot.svg?style=shield)](https://circleci.com/gh/saschagrunert/performabot)
[![Coverage](https://coveralls.io/repos/github/saschagrunert/performabot/badge.svg?branch=master)](https://coveralls.io/github/saschagrunert/performabot?branch=master)
[![Doc](https://img.shields.io/badge/doc-performabot-orange.svg)](https://saschagrunert.github.io/performabot)
[![License MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/saschagrunert/performabot/blob/master/LICENSE)

Welcome to performabot! This little helper can be used to provide Continuous
Performance Reports within your GitHub project.

## How it works

## Depdendencies

There is only one dependency needed to get started with this project:
[nix](https://nixos.org/nix)

To build the project, simply run:

```shell
> make
```

If you need a shell where all build dependencies are already in `$PATH`, then
run:

```shell
> make shell
```

There are other useful targets within the [Makefile](Makefile), which are used
by the CI and could be worth a look.

## Contributing

You want to contribute to this project? Wow, thanks! So please just fork it and
send me a pull request.
