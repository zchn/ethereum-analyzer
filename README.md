[![Build Status](https://travis-ci.org/zchn/ethereum-analyzer.svg?branch=master)](https://travis-ci.org/zchn/ethereum-analyzer)


## Usage

``` shell
stack build
solc --combined-json ast examples/analysis-benchmark/selfdestruct-over-suicide.sol | stack exec ea-analyze
```
