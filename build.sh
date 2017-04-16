#!/bin/bash
docker run -it --expose 8545 -v `pwd`:/ethereumH-dev --rm zchn/ethereumk:sandbox "/ethereumH-dev/build_in_docker.sh"
