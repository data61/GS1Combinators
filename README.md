# GS1 Combinator Library

A library to parse the GS1 Events into Haskell data types.

## Documentation

[View the Haskell API documentation here](docs/index.html)

Alternatively, the documentation can be generated locally using the following command

```
./generate_docs.sh
```

and then viewing `docs/index.html` in a browser.

## Getting Started

### Compiling

Download and install the Haskell build tool, [stack](https://www.haskellstack.org/), and run

```
stack build
```

### Running Tests

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

## GS1 Standards

The following standards and documents have been consulted while developing this library:

- [GS1 General Specifications](https://www.gs1.org/docs/barcodes/GS1_General_Specifications.pdf)
- [GS1 System Architecture Document](https://www.gs1.org/docs/architecture/GS1_System_Architecture.pdf)
- [GS1 System Landscape](https://www.gs1.org/docs/architecture/GS1_System_Landscape.pdf)
- [Core Business Vocabulary Standard 1.2](https://www.gs1.org/sites/default/files/docs/epc/CBV-Standard-1-2-r-2016-09-29.pdf)
- [EPC Information Services (EPCIS) Standard 1.2](https://www.gs1.org/sites/default/files/docs/epc/EPCIS-Standard-1.2-r-2016-09-29.pdf)
- [EPCIS and CBV Implementation Guideline](https://www.gs1.org/docs/epc/EPCIS_Guideline.pdf)
- [EPC Tag Data Standard](https://www.gs1.org/sites/default/files/docs/epc/GS1_EPC_TDS_i1_11.pdf)
