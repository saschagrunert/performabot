# Performabot

[![CircleCI](https://circleci.com/gh/saschagrunert/performabot.svg?style=shield)](https://circleci.com/gh/saschagrunert/performabot)
[![Coverage](https://coveralls.io/repos/github/saschagrunert/performabot/badge.svg?branch=master)](https://coveralls.io/github/saschagrunert/performabot?branch=master)
[![Doc](https://img.shields.io/badge/doc-performabot-orange.svg)](https://saschagrunert.github.io/performabot)
[![License MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/saschagrunert/performabot/blob/master/LICENSE)

Welcome to Performabot! This little helper can be used to provide Continuous
Performance Reports within your GitHub project. But please be aware that
currently only [ginkgo (Go)](https://onsi.github.io/ginkgo/#benchmark-tests)
performance benchmarks are supported.

## How it works

Performabot parses the output of a micro-benchmarking framework. After the
successful parsing it puts the data into a new GitHub repository called
`performabot-results`. This repository is automatically created by the user
who owns the specified token. All data within that repository will be managed by
performabot, whereas data updates result in additional commits within that
repository. If Performabot finds data where it can compare the current run
against, then it will do that as well.

Performabot updates the corresponding GitHub pull request after the run with a
comment which contains all necessary information, like these:

```diff
@@                  Performance Diff                    @@
##                                        Ø       ±   × ##
==========================================================
  create Container                   0.122s  0.035s  20
  start Container                    0.030s  0.012s  20
  stop Container                     0.048s  0.014s  20
  remove Container                   0.026s  0.004s  20
==========================================================
- list PodSandbox                    0.000s  0.000s  20
+ list PodSandbox                    0.000s  0.000s  20
=                                    0.000s  0.000s   0
==========================================================
- list Container                     0.002s  0.005s  20
+ list Container                     0.000s  0.000s  20
=                                   -0.002s -0.005s   0
==========================================================
```

The test name, its average (Ø) and derivation (±) times and the amount (×) of
runs for the test is visible inside the table. Beside this, the difference
between two tests is being calculated as well if Performabot finds a base commit
to compare against.

During the run, Performabot needs some information about the local test
environment before actually commenting to GitHub pull requests. These parameters
can be specified either via local environment variables or the command line,
where test runs within [CircleCI](https://circleci.com) should work out of the
box, except for the token. The needed environmental information are:

| Flag                  | Environment Variable                        | Description                 |
| --------------------- | ------------------------------------------- | --------------------------- |
| `-c` `--commit`       | `$PB_COMMIT` `$CIRCLE_SHA1`                 | Commit hash                 |
| `-p` `--pull-request` | `$PB_PULL_REQUEST` `$CIRCLE_PR_NUMBER`      | Pull request number         |
| `-r` `--repository`   | `$PB_REPOSITORY` `$CIRCLE_PROJECT_REPONAME` | GitHub repository name      |
| `-o` `--owner`        | `$PB_OWNER` `$CIRCLE_PROJECT_USERNAME`      | GitHub repository owner     |
| `-t` `--token`        | `$PB_TOKEN`                                 | Personal access OAuth token |

Personal access tokens can be generated [via the GitHub developer
settings](https://github.com/settings/tokens), whereas only `public_repo`
(Access public repositories) has to be activated in case of an open source
project.

## Benchmark runs

As already mentioned, right now only
[ginkgo](https://onsi.github.io/ginkgo/#benchmark-tests) benchmarks are
supported. The tests have to be executed with `--succinct` to let the parser
finally work. Succinct silences much of Ginkgo’s more verbose output. Test
suites that succeed basically get printed out on just one line! Succinct is
turned off, by default, when running tests for one package. It is turned on by
default when Ginkgo runs multiple test packages.

Feel free to open an issue or pull request for additional parsing support.

## Try it out

If you want to give the latest master branch a quick try, then install a
container runtime like [podman](https://podman.io) and test it with the
[`test/sample`](test/sample) file:

```shell
> cat test/sample | podman run -i saschagrunert/performabot -l
...
[NOTE]: Welcome to performabot!
...
[NOTE]: Processing done, found 16 results
[NOTE]: Local run specified
[NOTE]: The report:

### Performabot Result 🤖

Nothing to compare against.

@@                  Performance Diff                    @@
##                                        Ø       ±   × ##
==========================================================
  create PodSandbox                  0.404s  0.471s  20
  PodSandbox status                  0.000s  0.000s  20
  stop PodSandbox                    0.240s  0.015s  20
  remove PodSandbox                  0.056s  0.012s  20
  create PodSandbox and container    0.405s  0.049s  20
  list Container                     0.002s  0.005s  20
  pull Image                         6.498s  0.478s  20
  Image status                       0.002s  0.001s  20
  remove Image                       0.068s  0.044s  20
  create Container                   0.122s  0.035s  20
  start Container                    0.030s  0.012s  20
  Container status                   0.002s  0.001s  20
  stop Container                     0.048s  0.014s  20
  remove Container                   0.026s  0.004s  20
  list PodSandbox                    0.000s  0.000s  20
  list Container                     0.000s  0.000s  20
==========================================================
```

The `-l` `--local` flag specifies a local run. This means that there will be no
validation of the local environment variables nor any GitHub interaction at all.
Nevertheless, a local `performabot.sqlite` should now be available within the
current directory which contains the run.

Further command line arguments are:

```
Usage: performabot [-c|--commit COMMIT] [-p|--pull-request PULL_REQUEST]
                   [-r|--repository REPOSITORY] [-o|--owner OWNER]
                   [-t|--token TOKEN] [-v|--verbose] [-l|--local] [--version]

Available options:
  -c,--commit COMMIT       Commit hash - fallback environment variables:
                           $PB_COMMIT, $CIRCLE_SHA1
  -p,--pull-request PULL_REQUEST
                           Pull request number - fallback environment variables:
                           $PB_PULL_REQUEST, $CIRCLE_PR_NUMBER
  -r,--repository REPOSITORY
                           GitHub repository - fallback environment variables:
                           $PB_REPOSITORY, $CIRCLE_PROJECT_REPONAME
  -o,--owner OWNER         GitHub owner - fallback environment variables:
                           $PB_OWNER, $CIRCLE_PROJECT_USERNAME
  -t,--token TOKEN         Token - fallback environment variable: $PB_TOKEN
  -h,--help                Show this help text
  -v,--verbose             Logging verbosity, can be specified up to 2x
  -l,--local               Run only locally and do not upload anything
  --version                Print the current version
  -h,--help                Show this help text
```

## Build dependencies

There is only one dependency needed to get started with this project:
[nix](https://nixos.org/nix)

To build the project, simply run:

```shell
> make
```

The binary should be available at `result/bin/performabot` after successful
compilation. Before running that binary please ensure that you have the
`$LOCALE_ARCHIVE` environment variable set. This is done automatically on
[NixOS](https://nixos.org) and nix shells, but it does not apply to other
environments. The repository contains two helper scripts which can be sourced
from [bash](https://www.gnu.org/software/bash) or [fish](https://fishshell.com)
to ensure this:

```shell
. hack/env.fish
```

```shell
. hack/env.sh
```

If you need a shell where all build time dependencies are already
included in `$PATH`, then run:

```shell
> make shell
```

There are other useful targets within the [Makefile](Makefile), which are used
by the CI and could be worth a look. If you don’t want to use nix, then you’re
free to build the project with [stack](https://haskellstack.org) or
[cabal](https://www.haskell.org/cabal) as well.

## Contributing

You want to contribute to this project? Wow, thanks! So please just fork it and
send me a pull request.
