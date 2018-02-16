# GS1Combinators

### A library to parse the GS1 Events into Haskell data types.

You must always remember to build before testing, since the automatic building before the test building has been removed.

This is automated in run_tests.sh

`./run_tests.sh`

To perform a clean beforehand, use the -c option

`./run_tests.sh -c`

To clean, run `stack clean`

To build, run `stack build`

To test, run `stack test`

To parse an XML file, run `stack exec Parser-exe -- /path/to/XML/file`

To check the validity/expected output of the parsing of an XML file in terms of GS1 vocabulary,
use this [tool](http://www.vizworkbench.com/ui/dataset/).
You  may be asked to create an account.

# Info about GS1Combinators

Information about the standard comes from the following documents:

doc/CBV-Standard-1-2-r-2016-09-29.pdf

doc/EPCIS_Guideline.pdf

doc/EPCIS-Standard-1.2-r-2016-09-29.pdf

doc/GS1_EPC_TDS_i1_11.pdf

GS1Combinators contains types representing all the business events.

The files in "test/test-xml/EPCIS xml files" contain additional examples sent to us by the GS1 team, not present in the documentation.

# Dimensions

There are What, When, Where and Why dimensions for business events, representing all information about the event. They each represent their obvious meaning.

# Tests

The tests filenames correspond to the module they test. They generally parse XML and check the result.