# GS1Combinators

### A library to parse the GS1 Events into Haskell data types.

You must always remember to build before testing, since the automatic building before the test building has been removed.

This is automated in run_tests.sh

`./run_tests.sh`

To perform a clean beforehand, use the -c option

`./run_tests.sh -c`

To build, run `stack build`

To test, run `stack test`

To parse an XML file, run `stack exec Parser-exe -- /path/to/XML/file`

To check the validity/expected output of the parsing of an XML file in terms of GS1 vocabulary,
use this [tool](http://www.vizworkbench.com/ui/dataset/).
You  may be asked to create an account.
