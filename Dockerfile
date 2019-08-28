# docker container for nitta project building and testing
# sudo docker build --build-arg http_proxy=http://proxy.ifmo.ru:3128 --build-arg HTTP_PROXY=http://proxy.ifmo.ru:3128 -t nitta-build .

FROM ubuntu:18.04
ENV DEBIAN_FRONTEND noninteractive

# haskell stack

RUN apt-get update && apt-get install -y \
	haskell-stack netbase git libghc-zlib-dev libghc-zlib-bindings-dev \
	iverilog

RUN stack upgrade && \
	echo "export PATH=/root/.local/bin:$PATH" >> ~/.profile

ENV \
	RESOLVER=lts-13.27 \
	PATH="/root/.local/bin:${PATH}" \
	LANG=C.UTF-8

# nitta deps

ADD stack.yaml nitta.cabal /data/
WORKDIR /data

RUN stack setup --resolver ${RESOLVER}
RUN stack --resolver ${RESOLVER} build --only-dependencies --haddock --test

# npm

RUN apt-get install -y npm && npm install -g npm

ADD web/package.json web/package-lock.json /data/
RUN npm install --only=dev --global
