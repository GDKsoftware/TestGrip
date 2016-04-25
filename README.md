# TestGrip

With TestGrip you can create unit tests for your methods without creating a special DUnit or DUnitX project.

## How to use
For information on how to use TestGrip in your development, we have made some [useful screencasts](http://www.gdcsoftware.com/index.php/testgrip/screenshots-and-screencasts/).

## Main Application suite
* TestGripPlugin - The TestGrip Delphi IDE plugin
* TestGripTester - Commandline application to run tests made in TestGrip

## Secondary
These applications mainly use and demonstrate the TestGrip Core for project and unit parsing.
* ImpGen - Generate a new implementation of an interface
* TestIDE - Simulated IDE to preview the plugin
* UnitBrowser - To view the structure of a given .pas file
* UnitSearch - To search a project and in which units a given unit is used

### ImpGen
You can add ImpGen and some of the other applications as tools in the Delphi IDE. For ImpGen the best way is to setup a tool with the Parameters: $PROJECT $CURTOKEN

## Supported Delphi versions
Older Delphi versions still need some additional testing and will be added soon.

[x] Delphi Seattle
[x] Delphi Seattle Trial (cannot build or run tests)
