FROM ubuntu:20.10
ENV DEBIAN_FRONTEND noninteractive

ENV \
    PATH="/root/.local/bin:${PATH}" \
    LANG=C.UTF-8 \
    NODE_MODULES_PATH="/tmp/node_modules"

RUN apt-get update && apt-get install -y \
    haskell-stack netbase git libghc-zlib-dev libghc-zlib-bindings-dev \
    iverilog npm yarn

RUN stack upgrade && \
    echo "export PATH=/root/.local/bin:$PATH" >> ~/.profile

RUN npm install -g npm
RUN npm install -g yarn

WORKDIR /data

ADD stack.yaml stack.yaml.lock nitta.cabal package.yaml /data/
RUN stack setup && stack build --only-dependencies --haddock --test

ADD web/package.json web/yarn.lock /data/web/
RUN yarn --cwd web install --modules-folder $NODE_MODULES_PATH
