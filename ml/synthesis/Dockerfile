# Dockerfile bundling together all NITTA + ML deps for development and artifact building purposes
# Build context is expected to be a repository root:
#   docker build -f path/to/this/Dockerfile .
# 
# The following build stages structure is chosen:
#
#     ubuntu:20.04 <-- dependencies <-- build <-- ml-script
#                           ^--- development <-- development-gpu
#
#     - "build" builds just NITTA itself
#     - "ml-script" adds and prepares ML-related stuff
#     - "development" is a base stage for development containers
#
# During development it's useful to add a bind mount at /app to the repo root. 
# NITTA source code is not added to the image during "dependencies" build stage to avoid breaking build cache for "build" stage which depends on it.
# Create another stage deriving from "dependencies" if practice shows we really need source code in the image instead of a bind mount.

# if you change the ubuntu version, don't forget to update the CUDA repository in development containers below
FROM ubuntu:20.04 as dependencies

# constructing environment in a layered fashion to utilize docker build cache

# ---- non-root user setup ----
# Non-root is needed:
# - as a security best practice for running stuff in containers
# - to avoid permission issues with bind mounts during development
# - many tools are designed to be installed/used by non-root users:
#     - pip warns that installing packages as root may break system packages
#     - ghcup is designed to be installed and used as a non-root user (installs to ~/.ghcup, tweaks path in ~/.bashrc, etc.)
#     - stack's build cache is per-user

# dev bind mounts must not break file permissions in the host system, so we need to create a user with the same uid/gid as the host user
# BUT it's relevant only to linux / windows-via-wsl2 since on macos osxfs automatically maps container file ownership
ARG HOST_UID
ARG HOST_GID
ENV HOST_UID=${HOST_UID}
ENV HOST_GID=${HOST_GID}
# --gid and --uid arguments are only specified if they were passed as build args
# ${var:+foo $var} -> "foo bar" if var is set ("bar" in this example), empty string otherwise
RUN groupadd ${HOST_GID:+--gid $HOST_GID} devuser \
    && useradd ${HOST_UID:+--uid $HOST_UID} --gid devuser --shell /bin/bash --create-home devuser
USER devuser
RUN echo "cd /app" >> "/home/devuser/.profile"
USER root
# sudo is needed only for development images (and is a security risk), 
# so we'll omit its installation in images for non-interactive containers and install it later in development images

# -- initializing app dir --
# it will be application's workdir and repo root
RUN mkdir /app && chown -R devuser:devuser /app
WORKDIR /app

# ---- install build tools ----
# noninteractive is needed to avoid interactive prompts during apt-get install from packages like tzdata
ENV DEBIAN_FRONTEND=noninteractive

# -- common --
RUN apt-get update -yq \
    # do we really need an upgrade here?
    && apt-get upgrade -y \
    && apt-get install apt-utils curl -yq

# git is also needed for stack build plan to work
# updating it so that CVE-2022-24765 is fixed (and source control tools don't complain during development...)
RUN apt-get install -yq software-properties-common \
    && add-apt-repository -y ppa:git-core/ppa \
    && apt-get update \
    && apt-get install -yq git

# -- haskell --
# GHCup (https://www.haskell.org/ghcup/) is used to install GHC, Stack, etc.
# easier to control versions, practically required for development images
RUN apt-get install -yq build-essential curl libffi-dev libffi7 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5
USER devuser
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
    BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    # GHC 9.4.4 is needed for fourmolu-0.10.1.0. can also be: latest | recommended (default)
    BOOTSTRAP_HASKELL_GHC_VERSION=9.4.4 \
    BOOTSTRAP_HASKELL_STACK_VERSION=recommended \
    # DO NOT prepend ghcup's binaries dir to PATH in ~/.bashrc
    # we'll do it later manually in a way that includes non-interactive ssh shells (required for development)
    # BOOTSTRAP_HASKELL_ADJUST_BASHRC=1 \
    sh

# adding ghcup binaries to PATH
ENV PATH="/home/devuser/.ghcup/bin:$PATH"

# setting up ghcup-stack integration via stack hooks, see https://www.haskell.org/ghcup/guide/#stack-integration
RUN mkdir -p ~/.stack/hooks/ \
    && curl https://raw.githubusercontent.com/haskell/ghcup-hs/master/scripts/hooks/stack/ghc-install.sh \
        > ~/.stack/hooks/ghc-install.sh \
    && chmod +x ~/.stack/hooks/ghc-install.sh \
    # hooks are only run when 'system-ghc: false'
    && stack config set system-ghc false --global \
    # do not fallback to stack ghc installation if ghcup is not available
    && stack config set install-ghc false --global \
    # update resolver in the implicit global stack project (needed for GHC 9.4.4 for fourmolu-0.10.1.0)
    && stack --resolver nightly-2023-04-09 clean

# setting up PATH in .bashrc to include ghcup binaries (minding non-interactive ssh shells, see inserted comment below)
# (sed inserts given text before the matched comment (which should be there by default)
# if we start a line with #, it gets stripped by Dockerfile parser, so putting \n at the beginning of each line to implement comments
RUN sed -i '/# If not running interactively,/i # ---- custom non-interactive section start ---- \
\n\
\n# We do it here BEFORE checking if the shell is interactive because \
\n# VS Code runs its server in a non-interactive non-login shell created by ssh \
\n# and those vars are still needed. .bashrc is still executed in this case. \
\n\
\n# ghcup-env \
\n[ -f "/home/devuser/.ghcup/env" ] && source "/home/devuser/.ghcup/env" \
\n\
\n# ---- custom non-interactive section end ---- \
\n\
' ~/.bashrc

RUN echo "GHCUP: $(ghcup --version)" \
    && echo "STACK: $(stack --version)" \
    && echo "GHC: $(ghc --version)"

USER root

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
USER devuser

# -- haskell --
COPY --chown=devuser:devuser nitta.cabal stack.yaml stack.yaml.lock ./
RUN stack build --only-dependencies

# -- frontend --
WORKDIR /app/web
COPY --chown=devuser:devuser web/package.json web/yarn.lock ./
RUN yarn install

# -- ml --
WORKDIR /app/ml/synthesis
COPY --chown=devuser:devuser ml/synthesis/requirements.txt ./
RUN pip3 install --user -r requirements.txt \
    && pip3 cache purge


# ---- finalizing ----
WORKDIR /app
USER devuser

# -----------------------------


FROM dependencies AS development
# ---- target for spinning up a container with development envoronment ---
# Bind mount of repo root to workdir is expected here.
# Using them to map live source code from the host filesystem straight into the container. 
# Container will pick changes made during development without docker image rebuilds. 
# Existing container data will be obscured (https://docs.docker.com/storage/bind-mounts/#mount-into-a-non-empty-directory-on-the-container), this is fine.

USER root

# -- installing/configuring development tools --
# sudo is needed for root operations under devuser
RUN apt-get install -yq sudo \
    && usermod -aG sudo devuser \
    # it will be passwordless
    && echo "devuser ALL=(ALL) NOPASSWD:ALL" | (EDITOR='tee -a' visudo)

# unminimizing the system is required to get a decent teminal experience and dev environment
# installing common handy dev tools here too
# git is already installed and updated earlier
RUN yes | unminimize \
    && apt-get install -yq man-db htop vim

# installing NITTA dev dependencies
RUN apt-get install -yq iverilog gtkwave libtinfo-dev

USER devuser
WORKDIR /home/devuser
# this used to fail with linker errors without libtinfo-dev installed while building ghc-lib-parser-9.0.2
# fourmolu-0.10.1.0 is pinned because of https://github.com/ryukzak/nitta/issues/242
RUN stack install hlint fourmolu-0.10.1.0
WORKDIR /app
USER root

# installing and configuring ssh server for remote debugging
RUN apt-get install -yq screen openssh-server
RUN mkdir /var/run/sshd \
    && sed -i 's/#PermitRootLogin prohibit-password/PermitRootLogin yes/' /etc/ssh/sshd_config

USER devuser

# GPG commit signing can be troublesome in dev containers
# proposed workaround is a prolonged gpg-agent passphrase timeout and a helper script to enter the passphrase via terminal
# default is 3 hours = 3600 * 3 = 10800 seconds
ARG GPG_PASSPHRASE_CACHE_TTL=10800
RUN mkdir -p ~/.gnupg \
    && echo "use-agent" >> ~/.gnupg/gpg.conf \
    && echo "default-cache-ttl ${GPG_PASSPHRASE_CACHE_TTL}" >> ~/.gnupg/gpg-agent.conf \
    && echo "max-cache-ttl ${GPG_PASSPHRASE_CACHE_TTL}" >> ~/.gnupg/gpg-agent.conf \
    && echo "#!/bin/bash\
\nexport GPG_TTY=\$(tty)\
\necho test | gpg --sign > /dev/null" >> ~/passphr.sh \
    && chmod +x ~/passphr.sh \
    && chmod 600 ~/.gnupg/* \
    && chmod 700 ~/.gnupg

# needed for python code in docker-entrypoint-dev.sh
RUN pip3 install --user shutup

# prevent conflicts with previously build artifacts
RUN stack clean

# adding PYTHONPATH to .bashrc, including non-interactive ssh shells (like one spawned by vscode)
RUN sed -i '/# ---- custom non-interactive section end ----/i export PYTHONPATH=/app/ml/synthesis/src:$PYTHONPATH\n' ~/.bashrc

RUN echo "alias python=python3" >> ~/.bash_aliases \
    && echo "alias pip=pip3" >> ~/.bash_aliases \
    && echo "alias pass=~/passphr.sh" >> ~/.bash_aliases \
    && echo "PATH=$PATH:/home/devuser/.local/bin" >> ~/.profile 
    
ENTRYPOINT ["ml/synthesis/docker-entrypoint-dev.sh"]


# -----------------------------


FROM development as development-gpu
# ---- includes GPU support (tensorflow-gpu) for development container (possible only on Linux / Windows-WSL2 as of 2023.02)---
USER root


# ---- check if expected tensorflow version matches requirements.txt ----
# this should be in sync with version in requirements.txt
ARG TENSORFLOW_VER=2.12.0
# those should be in sync with TENSORFLOW_VER, taken from https://www.tensorflow.org/install/source#gpu
# cuDNN must be a 8.6 for this tensorflow version, but we also need to specify minor version. you can get it from "Available libcudnn8 versions" output below.
ARG CUDNN_VER=8.6.0.163
ARG CUDA_VER=11.8

RUN [ $(grep "tensorflow~=$TENSORFLOW_VER" ml/synthesis/requirements.txt | wc -l) = "1" ] || { echo "Tensorflow version mismatch, cannot continue. Tensorflow version was updated in requirements.txt, but not in the Dockerfile. Grab cuDNN and CUDA versions from https://www.tensorflow.org/install/source#gpu and update the Dockerfile near this check."; exit 1; } && \
    echo "Tensorflow version matches requirements.txt, reinstalling tensorflow $TENSORFLOW_VER with NVIDIA GPU support (cuDNN $CUDNN_VER, CUDA $CUDA_VER)."


# ---- installing GPUs-specific dependencies ----

# -- replacing tensorflow with tensorflow-gpu --
RUN pip3 uninstall -y tensorflow \
    && pip3 install --user "tensorflow-gpu~=$TENSORFLOW_VER" 

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
COPY --chown=devuser:devuser src src/
COPY --chown=devuser:devuser app app/
COPY --chown=devuser:devuser LICENSE ./
RUN stack build --local-bin-path $BUILD_OUTPUT_DIR/nitta --copy-bins nitta

# -- web --
COPY --chown=devuser:devuser web web/
RUN stack exec nitta-api-gen -- -v
WORKDIR /app/web
RUN yarn build

# ---- finalizing ----
WORKDIR /app


# -----------------------------


FROM build AS ml-script

COPY --chown=devuser:devuser examples examples/
COPY --chown=devuser:devuser ml ml/
ENV PYTHONPATH=/app/ml/synthesis/src
ENV PYTHONUNBUFFERED=1
ENTRYPOINT ["python3"]
