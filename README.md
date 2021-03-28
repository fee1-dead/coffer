# Coffer

Licensed under LGPL v3 or later, a copy of the Lesser GNU General Public License 
can be found by the name of `LICENSE.md`

## Running tests

Use `cargo test` to run tests that do not require java.

Tests that require java are ignored by default, to add them, make sure you have 
`java` in your path and run `cargo test -- --include-ignored` (stable 1.51 and above).