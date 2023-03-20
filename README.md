# Coffer - a library for reading and writing Java classes

Except as otherwise noted (in individual files or in <COPYRIGHT.md>), Coffer is
licensed under the Apache License, Version 2.0 <LICENSE-APACHE> or
<http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
<LICENSE-MIT> or <http://opensource.org/licenses/MIT>, at your option.

NOTE: This library is not actively maintained, but there are still many features missing.
If you'd like to help implement this feature feel free to reach out to me on discord
`fee1-dead#7913` or on matrix `@deadbf:matrix.org`. One feature that needs more work is
performing static analysis so that we can automatically generate the maximum stack and 
local variables as well as the StackMapTable.

## Running tests

Use `cargo test` to run tests that do not require java.

Tests that require java are ignored by default, to add them, make sure you have 
`java` in your path and run `cargo test -- --include-ignored` (stable 1.51 and above).
