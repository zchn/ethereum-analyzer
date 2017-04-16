#!/bin/bash
cd /ethereumH-dev
# /usr/bin/stack --system-ghc install hscolour
# /usr/bin/stack --system-ghc haddock
/usr/bin/stack --system-ghc build && .stack-work/install/x86_64-linux/lts-3.4/7.10.2/bin/ethereum-analyzer
curl -X POST --data '{"jsonrpc":"2.0","params":[],"method":"web3_clientVersion","id":1}' -H 'content-type:application/json' http://192.168.3.53:8545
