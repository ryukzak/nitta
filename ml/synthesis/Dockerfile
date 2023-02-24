# Dockerfile bundling together all NITTA + ML deps for development and artifact building purposes
# Build context is expected to be a repository root:
# 	docker build -f path/to/this/Dockerfile .
# 
# During development it's useful to add a bind mount at /app to the repo root. 
# NITTA source code is not added to the image during "dependencies" build stage to avoid breaking build cache for "build" stage which depends in it.
# Create another stage deriving from "dependencies" if practice shows we really need source code in the image instead of a bind mount.

# if you change the ubuntu version, don't forget to update the CUDA repository below
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
	&& python3 -m pip install --upgrade pip \
	&& pip3 -V


# ---- get build dependencies ready ----

# -- haskell --
COPY nitta.cabal stack.yaml stack.yaml.lock ./
# RUN stack build --only-dependencies  # skipping that since we need to rebuild it from non-root in development image user anyway

# -- frontend --
WORKDIR /app/web
COPY web/package.json web/yarn.lock ./
RUN yarn install

# -- ml --
WORKDIR /app/ml/synthesis
COPY ml/synthesis/requirements.txt ./
RUN pip3 install -r requirements.txt \
	&& pip3 cache purge


# ---- finalizing ----
WORKDIR /app 


# -----------------------------


FROM dependencies AS development
# ---- target for spinning up a container with development envoronment ---
# Bind mount of repo root to workdir is expected here.
# Using them to map live source code from the host filesystem straight into the container. 
# Container will pick changes made during development without docker image rebuilds. 
# Existing container data will be obscured (https://docs.docker.com/storage/bind-mounts/#mount-into-a-non-empty-directory-on-the-container), this is fine.

# -- installing/configuring development tools --
# dev bind mounts must not break file permissions in the host system, so we need to create a user with the same uid/gid as the host user
ARG HOST_UID
ARG HOST_GID
RUN apt-get install -yq sudo \
	&& groupadd --gid $HOST_GID devuser \
  	&& useradd --uid $HOST_UID --gid devuser --shell /bin/bash --create-home devuser \
  	&& usermod -aG sudo devuser \
  	&& echo "devuser ALL=(ALL) NOPASSWD:ALL" | (EDITOR='tee -a' visudo) \ 
  	&& echo "cd /app" >> "/home/devuser/.profile"
# sudo for this user will be passwordless

# removing traces of root's stack and chowning the whole app to devuser.
# this may take a while due to a large number of files to chown. 
# any ways to speed up or avoid this?
RUN rm -rf /root/.stack && chown -R devuser:devuser /app

# pre-building haskell dependencies and downloading ghc into devuser's .stack workdir.
USER devuser
RUN stack build --only-dependencies
USER root

# installing NITTA dev dependencies
# noninteractive is needed to avoid interactive prompts during apt-get install from packages like tzdata
ENV DEBIAN_FRONTEND=noninteractive   
RUN apt-get install -yq iverilog gtkwave libtinfo-dev

USER devuser
# this fails with linker errors without libtinfo-dev installed (ghc-lib-parser-9.0.2)
RUN stack install hlint fourmolu
USER root

# needed for python code in docker-entrypoint-dev.sh
RUN pip3 install shutup

# installing and configuring ssh server for remote debugging + convenient container dev tools
RUN apt-get install -yq screen htop openssh-server
RUN mkdir /var/run/sshd \
	&& sed -i 's/#PermitRootLogin prohibit-password/PermitRootLogin yes/' /etc/ssh/sshd_config

# updating git so that CVE-2022-24765 is fixed and source control tools don't complain
RUN apt-get install -yq software-properties-common \
	&& add-apt-repository -y ppa:git-core/ppa \
	&& apt-get update \
	&& apt-get install -yq git

USER devuser

RUN echo "export PYTHONPATH=/app/ml/synthesis/src:$PYTHONPATH" >> "/home/devuser/.profile" \
	&& echo "alias python=python3" >> "/home/devuser/.profile" \
	&& echo "alias pip=pip3" >> "/home/devuser/.profile" \
	&& echo "PATH=$PATH:/home/devuser/.local/bin" >> "/home/devuser/.profile" 
	
ENTRYPOINT ["ml/synthesis/docker-entrypoint-dev.sh"]


# -----------------------------


FROM development as development-gpu
# ---- includes GPU support (tensorflow-gpu) for development container (possible only on Linux / Windows-WSL2 as of 2023.02)---
USER root


# ---- check if expected tensorflow version matches requirements.txt ----
# this should be in sync with version in requirements.txt
ARG TENSORFLOW_VER=2.5.0
# those should be in sync with TENSORFLOW_VER, taken from https://www.tensorflow.org/install/source#gpu
# cuDNN must be a 8.1 for this tensorflow version, but we also need to specify minor version. you can get it from "Available libcudnn8 versions" output below.
ARG CUDNN_VER=8.1.1.33
ARG CUDA_VER=11.2

RUN [ $(grep "tensorflow~=$TENSORFLOW_VER" ml/synthesis/requirements.txt | wc -l) = "1" ] || { echo "Tensorflow version mismatch, cannot continue. Tensorflow version was updated in requirements.txt, but not in the Dockerfile. Grab cuDNN and CUDA versions from https://www.tensorflow.org/install/source#gpu and update the Dockerfile near this check."; exit 1; } && \
	echo "Tensorflow version matches requirements.txt, reinstalling tensorflow $TENSORFLOW_VER with NVIDIA GPU support (cuDNN $CUDNN_VER, CUDA $CUDA_VER)."


# ---- installing GPUs-specific dependencies ----

# -- replacing tensorflow with tensorflow-gpu --
RUN pip3 uninstall -y tensorflow \
	&& pip3 install "tensorflow-gpu~=$TENSORFLOW_VER" 

# -- installing CUDA Toolkit required for tensorflow-gpu --
RUN apt-get install -yq wget \
	&& wget https://developer.download.nvidia.com/compute/cuda/repos/ubuntu2004/x86_64/cuda-keyring_1.0-1_all.deb \
	&& dpkg -i cuda-keyring_1.0-1_all.deb \
	&& rm cuda-keyring_1.0-1_all.deb \
	&& apt-get update -yq

# cache image as a separate layer before installation in case it will be needed. the previous RUN layer executes in 5+ mins.
RUN apt-get install -yq cuda-toolkit-$CUDA_VER

# -- installing cuDNN required for tensorflow-gpu --
RUN echo "Available libcudnn8 versions (for reference if changing cuDNN version):" \
	&& apt-cache madison libcudnn8 \
	&& apt-get install -yq libcudnn8=$CUDNN_VER-1+cuda$CUDA_VER

USER devuser


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
ENV PYTHONPATH=/app/ml/synthesis/src
ENV PYTHONUNBUFFERED=1
ENTRYPOINT ["python3"]
