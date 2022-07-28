# Coffer - a lightweight and fast library for reading and writing Java classes

Except as otherwise noted (in individual files or in <COPYRIGHT.md>), Coffer is
licensed under the Apache License, Version 2.0 <LICENSE-APACHE> or
<http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
<LICENSE-MIT> or <http://opensource.org/licenses/MIT>, at your option.

## Running tests

Use `cargo test` to run tests that do not require java.

Tests that require java are ignored by default, to add them, make sure you have 
`java` in your path and run `cargo test -- --include-ignored` (stable 1.51 and above).
