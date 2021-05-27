# Coffer - a lightweight and fast library for reading and writing Java classes

[![Discord](https://img.shields.io/discord/721385143461478480?logo=discord)](https://discord.gg/Ddv9XUPYcK)
[![pipeline status](https://gitlab.com/fee1-dead/coffer/badges/master/pipeline.svg)](https://gitlab.com/fee1-dead/coffer/-/commits/master)

Licensed under LGPL v3 or later, a copy of the Lesser GNU General Public License 
can be found by the name of `LICENSE.md`

## Running tests

Use `cargo test` to run tests that do not require java.

Tests that require java are ignored by default, to add them, make sure you have 
`java` in your path and run `cargo test -- --include-ignored` (stable 1.51 and above).
