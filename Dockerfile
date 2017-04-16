FROM        ubuntu:xenial
MAINTAINER  Kevin Chen <k_@berkeley.edu>

# Setup environment.
ENV PATH /opt/ghc/bin:$HOME/.local/bin:$PATH

# Default command on startup.
CMD bash

# Setup packages.
RUN apt-get update && apt-get -y install git postgresql postgresql-server-dev-all libleveldb-dev curl autoconf libtool
RUN curl -sSL https://get.haskellstack.org/ | sh

RUN git clone https://github.com/ethereumK/ethereumH && cd ethereumH && git submodule init && git submodule update
RUN cd /ethereumH/ && stack upgrade && stack setup && stack build
