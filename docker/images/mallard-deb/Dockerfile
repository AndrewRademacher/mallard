FROM ubuntu:16.04

RUN apt-get update
RUN apt-get install wget libpq-dev -y

WORKDIR root

RUN wget -qO- https://get.haskellstack.org/ | sh

# Copy stack root.
COPY ./stack.yaml /root/stack.yaml

# Copy cabal file.
COPY ./mallard.cabal /root/mallard.cabal

RUN stack setup
RUN stack build --only-dependencies

# Copy source materials and build.
COPY . /root/
RUN stack build