# Dockerfile bundling together all NITTA + ML deps for development and artifact building purposes
# Build context is expected to be a repository root:
# 	docker build -f path/to/this/Dockerfile .
# 
# During development it's useful to add a bind mount at /app to the repo root. 
# NITTA source code is not added to the image during "dependencies" build stage to avoid breaking build cache for "build" stage which depends in it.
# Create another stage deriving from "dependencies" if practice shows we really need source code in the image instead of a bind mount.

FROM ubuntu:20.04 as dependencies

WORKDIR /app

# constructing environment in a layered fashion to utilize docker build cache


# ---- install build tools ----

# -- common --
RUN apt-get update -yq \
	&& apt-get install apt-utils curl -yq

# -- haskell --
# installing stack on ubuntu manually instead of using fpco/stack-build since ubutnu seems to be more convenient for development
RUN curl -sSL https://get.haskellstack.org/ | sh \
	&& stack --version

# -- frontend --
RUN curl -sL https://deb.nodesource.com/setup_16.x | bash \
	&& apt-get install nodejs -yq \
	&& node --version \
	&& npm --version \
	&& npm i -g yarn \
	&& yarn --version

# -- ml --
RUN python3 -V \
	&& apt-get install python3-pip -yq \
	&& pip3 -V


# ---- get build dependencies ready ----

# -- haskell --
COPY nitta.cabal stack.yaml stack.yaml.lock ./
RUN stack build --only-dependencies

# -- frontend --
WORKDIR /app/web
COPY web/package.json web/yarn.lock ./
RUN yarn install

# -- ml --
WORKDIR /app/ml/synthesis
COPY ml/synthesis/requirements.txt ./
RUN pip3 install -r requirements.txt


# ---- finalizing ----
WORKDIR /app 


# -----------------------------


FROM dependencies AS build
# ---- copy and build NITTA source code ---
# need to list required directories manually here to avoid breaking build cache
# and every directory with a separate COPY instruction since otherwise it'll move directory contents, not directories itself. tedious.
# COPY with exclusions is still not possible (https://github.com/moby/moby/issues/15771)
# a workaround would be to archive needed files on host before calling docker build and unpacking them here, 
# but it seems better for now to keep a simple single Dockerfile to build with.

ARG BUILD_OUTPUT_DIR=build
RUN mkdir $BUILD_OUTPUT_DIR

# -- haskell --
COPY src src/
COPY app app/
COPY LICENSE ./
RUN stack build --local-bin-path $BUILD_OUTPUT_DIR/nitta --copy-bins nitta

# -- web --
COPY web web/
RUN stack exec nitta-api-gen -- -v
WORKDIR /app/web
RUN yarn build

# ---- finalizing ----
WORKDIR /app


# -----------------------------


FROM build AS ml-script

COPY examples examples/
COPY ml ml/
WORKDIR /app
ENV PYTHONPATH=/app/ml/synthesis/src
ENV PYTHONUNBUFFERED=1
ENTRYPOINT ["python3"]
