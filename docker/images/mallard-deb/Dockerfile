FROM ubuntu:16.04

ENV DEBFULLNAME="Andrew Rademacher"
ENV DEBEMAIL="andrewrademacher@icloud.com"

RUN apt-get update
RUN apt-get install wget libpq-dev ruby ruby-dev rubygems build-essential -y
RUN gem install --no-ri --no-rdoc fpm

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

RUN mkdir -p /tmp/installdir/usr/local/bin
RUN cp $(stack exec -- which mallard) /tmp/installdir/usr/local/bin/mallard
RUN fpm -s dir -t deb -n mallard -v 0.6.2.0 -C /tmp/installdir \
        -d libpq5 \
        usr/local/bin/mallard

CMD cp mallard_0.6.2.0_amd64.deb /dist