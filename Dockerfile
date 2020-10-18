FROM ubuntu:20.10
ENV DEBIAN_FRONTEND noninteractive

ENV \
    PATH="/root/.local/bin:${PATH}" \
    LANG=C.UTF-8


RUN apt-get update && apt-get install -y \
    haskell-stack netbase git libghc-zlib-dev libghc-zlib-bindings-dev \
    iverilog npm

RUN stack upgrade && \
    echo "export PATH=/root/.local/bin:$PATH" >> ~/.profile

RUN npm install -g npm

WORKDIR /data

ADD stack.yaml stack.yaml.lock nitta.cabal package.yaml /data/
ADD web/package.json web/package-lock.json /data/web/

RUN stack setup && stack build --only-dependencies --haddock --test

RUN npm --prefix=web ci
