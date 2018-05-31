# GS1Combinators

## A library to parse the GS1 Events into Haskell data types.

You must always remember to build before testing, since the automatic building before the test building has been removed.

This is automated in run_tests.sh

`./run_tests.sh`

To perform a clean beforehand, use the -c option

`./run_tests.sh -c`

To clean, run `stack clean`

To build, run `stack build`

To test, run `stack test`

To parse an XML file, run `stack exec Parser-exe -- /path/to/XML/file`

More than one file can be parsed at a time. Just append to the arguments.

To check the validity/expected output of the parsing of an XML file in terms of GS1 vocabulary,
use this [tool](http://www.vizworkbench.com/ui/dataset/).
You  may be asked to create an account.

## Info about GS1Combinators

The documents specifying the implemented standard can be found in the directory `doc`.

GS1Combinators contains types representing all the business events.

The files in `test/test-xml/EPCIS xml files` contain additional examples
sent to us by the GS1 team, not present in the documentation.

## Dimensions

There are `What`, `When`, `Where` and `Why` dimensions for suppy chain events,
representing all information about the event.
