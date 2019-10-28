# Makefile commands reference

## General options:

- Default

  This option will build stack project, generate files for Web UI and build it.

- `prod`

  This option will built stack project, generate documentation, run tests and also will build Web UI.

- `build`

  This option will built stack project and generate documentation.

- `clean`

  This option will clean result directories and remove all running apps that are runned in the background.

- `debs`

  This option will install all dependencies from stack and Web UI projects.

## Advanced options

  More information you can see in Makefile

  - `npm.build`
  - `npm.dev`
  - `npm.install`
  - `stack.install`
  - `stack.prod`
  - `stack.build`
  - `stack.haddock`
  - `stack.gen`
  - `stack.web`

    Example: `make stack.web sim=fibonacci`

