FROM ubuntu:18.04
ENV DEBIAN_FRONTEND noninteractive

ENV \
    RESOLVER=lts-13.27 \
    PATH="/root/.local/bin:${PATH}" \
    LANG=C.UTF-8


RUN apt-get update && apt-get install -y \
    haskell-stack netbase git libghc-zlib-dev libghc-zlib-bindings-dev \
    iverilog npm

RUN stack upgrade && \
    echo "export PATH=/root/.local/bin:$PATH" >> ~/.profile

RUN npm install -g npm

WORKDIR /data

ADD Makefile stack.yaml nitta.cabal /data/
ADD web/package.json web/package-lock.json /data/web/

RUN make configure-stack

RUN make configure-npm && cp -Rv web/node_modules /tmp/node_modules

# for restore node_modules cache
# ln -s /tmp/node_modules web/node_modules
