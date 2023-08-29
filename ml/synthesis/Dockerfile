# Dockerfile bundling together all NITTA + ML deps for development and artifact building purposes
# Build context is expected to be a repository root:
#   docker build -f path/to/this/Dockerfile .
# 
# The following build stages structure is chosen:
#
#     ubuntu:22.04 <-- dependencies <-- build <-- ml-script
#                           ^--- development <-- development-gpu
#
#     - "build" builds just NITTA itself
#     - "ml-script" adds and prepares ML-related stuff
#     - "development" is a base stage for development containers
#
# During development it's useful to add a bind mount at /app to the repo root. 
# NITTA source code is not added to the image during "dependencies" build stage to avoid breaking build cache for "build" stage which depends on it.
# Create another stage deriving from "dependencies" if practice shows we really need source code in the image instead of a bind mount.

# if you change the Ubuntu version, don't forget to update the CUDA repository in the development-gpu target below
FROM ubuntu:22.04 as dependencies

# haskell tooling versions here are for ghcup
# can be: specific like "9.6.2" | "latest" | "recommended"
# default: recommended
# GHC 9.6.2 is needed for the recent fourmolu (0.13.1.0)
ARG GLOBAL_GHC_VER=9.6.2
ARG STACK_VER=recommended
# tried to get a fourmolu-0.12.0.0 like in CI, but stackage has no snapshot with such a version at the moment of writing:
# https://www.stackage.org/package/fourmolu/snapshots
# this resolver has fourmolu-0.13.1.0
ARG GLOBAL_STACK_RESOLVER=nightly-2023-08-23
ARG PYTHON_VER=3.11

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
# since user will run everything as a non-root user, sudo is required.
# only for development images though, and it's a security risk, so we'll omit its installation in images for 
# non-interactive containers and install it later in development images.

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
# easier to control versions, practically required for development images.
# !!! Currently recommended GHCup version is installed: it's better to have the latest version for development.
# !!! It may break stuff in the future. Pin the version if that becomes a pain. See lines with get-ghcup + ghcup-stack integration. 
RUN apt-get install -yq build-essential curl libffi-dev libffi7 libgmp-dev libgmp10 libncurses-dev libncurses5 \
        libtinfo5 libnuma1
# libnuma1 was added to fix linker errors (?) while building the image against aarch64
# the rest is ghcup deps from https://www.haskell.org/ghcup/install/#linux-ubuntu
USER devuser
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
    BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    BOOTSTRAP_HASKELL_GHC_VERSION=${GLOBAL_GHC_VER} \
    BOOTSTRAP_HASKELL_STACK_VERSION=${STACK_VER} \
    # DO NOT prepend ghcup's binaries dir to PATH in ~/.bashrc
    # we'll do it later manually in a way that includes non-interactive ssh shells (required for development with vscode)
    # BOOTSTRAP_HASKELL_ADJUST_BASHRC=1 \
    sh

# adding ghcup binaries to PATH for image building
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
    # update resolver in the implicit global stack project (needed for the recent fourmolu)
    # using this instead of "stack config" to **initialize** the global project (it's not created yet)
    && stack --resolver ${GLOBAL_STACK_RESOLVER} clean

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

USER devuser

# -- ml --
USER root
# Python build dependencies (pyenv requires them to build Python from source)
RUN apt-get install -yq build-essential libssl-dev zlib1g-dev libbz2-dev libreadline-dev libsqlite3-dev curl \
    libncursesw5-dev xz-utils tk-dev libxml2-dev libxmlsec1-dev libffi-dev liblzma-dev
USER devuser
# - pyenv -
# add PYENV_GIT_TAG=v2.3.24 before "bash" to pin the version
# skipping pinning since it's better for development to have the latest version, although it may break stuff
RUN curl https://pyenv.run | bash
ENV PYENV_ROOT="/home/devuser/.pyenv"
ENV PATH="/home/devuser/.pyenv/shims:/home/devuser/.pyenv/bin:$PATH"
RUN pyenv install ${PYTHON_VER} \
    && pyenv global ${PYTHON_VER} \
    && pip install --upgrade pip \
    && echo "=== === === PYTHON VERSION: " $(python -V) \
    && echo "=== === === PIP VERSION: " $(pip -V)
# - poetry -
# add POETRY_VERSION=1.6.1 before "python3" to pin the version
RUN curl -sSL https://install.python-poetry.org | python3 -
# poetry is installed to ~/.local/bin, which is not in PATH by default
ENV PATH="/home/devuser/.local/bin:$PATH"
# it's also added to PATH in .bashrc (see above) for VS Code Server's non-login non-interactive shell to see it
RUN echo "=== === === POETRY VERSION: " $(poetry --version) \
    # going to use pyenv's "system" python as venv since we're in a container
    # should have no problems with permissions since pyenv's python is located in devuser's home
    && poetry config virtualenvs.create false 


# ---- get build dependencies ready ----
USER root
# preparing stack build for aarch64
RUN apt-get install -yq clang lldb lld libnuma-dev
ENV LANG=C.UTF-8

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
COPY --chown=devuser:devuser ml/synthesis/poetry.lock ml/synthesis/pyproject.toml ./
RUN poetry install --no-root --only main --compile

# ---- finalizing ----
# set up PATH in .bashrc to include ghcup and pyenv binaries, yarn and poetry global installs, etc. (with non-interactive ssh shells in mind, see inserted comment below)
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
\nexport PYTHONPATH=/app/ml/synthesis/src:\
\n\
\n# adding local bin (poetry, first of all) to PATH\
\nexport PATH="$HOME/.local/bin:$PATH"\
\n\
\n# pyenv config: https://github.com/pyenv/pyenv\
\nexport PYENV_ROOT="$HOME/.pyenv"\
\ncommand -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"\
\neval "$(pyenv init -)"\
\n\
\n# yarn global installs location\
\nexport PATH="$(yarn global bin):$PATH"\
\n\
\n# ---- custom non-interactive section end ---- \
\n\
' ~/.bashrc

# solve buffering issues with python output for development and CI images
ENV PYTHONUNBUFFERED=1  

WORKDIR /app
USER devuser


# -----------------------------


FROM dependencies AS development
# A target to use for spinning up a container with development envoronment (without GPU support for ML).
# Bind mount of repo root to workdir is expected here! See related README.md for more info and usage examples.
# Using bind mounts to map live source code from the host filesystem straight into the container. 
# Container will pick changes made during development without docker image rebuilds. 
# Existing container data will be obscured (https://docs.docker.com/storage/bind-mounts/#mount-into-a-non-empty-directory-on-the-container), this is fine.

USER root

# -- installing/configuring development tools --
# sudo is needed for root operations under devuser
RUN apt-get install -yq sudo \
    && usermod -aG sudo devuser \
    # it will be passwordless
    && echo "devuser ALL=(ALL) NOPASSWD:ALL" | (EDITOR='tee -a' visudo)
# remove the sudo tutorial on startup
USER devuser
RUN touch ~/.sudo_as_admin_successful
USER root

# unminimizing the system is required to get a decent teminal experience and dev environment
# installing common handy dev tools here too
# git is already installed and updated earlier
RUN yes | unminimize \
    && apt-get install -yq man-db htop vim screen

# installing NITTA dev dependencies
RUN apt-get install -yq iverilog gtkwave
USER devuser
RUN yarn global add markdownlint-cli2

# pre-building stack test dependencies not to waste developer's time on that later
RUN stack build --test --only-dependencies
# install all python dependency groups (including dev ones)
WORKDIR /app/ml/synthesis
RUN poetry install --no-root --compile

# cd ~ to use the global stack project with the recent resolver and required (recent) fourmolu version
WORKDIR /home/devuser
RUN stack install hlint fourmolu
WORKDIR /app
USER root

# installing and configuring ssh server for remote development
RUN apt-get install -yq openssh-server
RUN mkdir /var/run/sshd \
    && sed -i 's/#PasswordAuthentication yes/PasswordAuthentication no/' /etc/ssh/sshd_config

USER devuser

# GPG commit signing can be troublesome in dev containers - IDEs may not support showing passphrase prompts.
# Proposed workaround is a prolonged gpg-agent passphrase timeout and a helper script to enter the passphrase via 
# terminal. Default timeout specified here is 3 hours (10800 seconds).
ARG GPG_PASSPHRASE_CACHE_TTL=10800
RUN mkdir -p ~/.gnupg \
    && echo "use-agent" >> ~/.gnupg/gpg.conf \
    && echo "default-cache-ttl ${GPG_PASSPHRASE_CACHE_TTL}" >> ~/.gnupg/gpg-agent.conf \
    && echo "max-cache-ttl ${GPG_PASSPHRASE_CACHE_TTL}" >> ~/.gnupg/gpg-agent.conf \
    && echo "#!/bin/bash\
\nexport GPG_TTY=\$(tty)\
\necho anystring | gpg --sign > /dev/null" >> ~/passphr.sh \
    && chmod +x ~/passphr.sh \
    && chmod 600 ~/.gnupg/* \
    && chmod 700 ~/.gnupg

# needed for python code in docker-entrypoint-dev.sh
RUN pip install --user shutup

# prevent conflicts with previous build artifacts
RUN stack clean

RUN echo "alias pass=~/passphr.sh" >> ~/.bash_aliases

ENTRYPOINT ["ml/synthesis/docker-entrypoint-dev.sh"]


# -----------------------------


FROM development as development-gpu
# ---- includes GPU support (tensorflow) for development container (possible only on Linux / Windows-WSL2 as of 2023.02)---
USER root


# ---- check if expected tensorflow version matches requirements.txt ----
# this should be in sync with version in requirements.txt
ARG TENSORFLOW_VER=2.12.*
# those should be in sync with TENSORFLOW_VER, taken from https://www.tensorflow.org/install/source#gpu
# cuDNN version must correspond to chosen tensorflow version, but we also need to specify minor version.
# you can get it from "Available libcudnn8 versions" output below.
ARG CUDNN_VER=8.6.0.163
ARG CUDA_VER=11.8

RUN [ $(grep "tensorflow = \"${TENSORFLOW_VER}" ml/synthesis/pyproject.toml | wc -l) = "1" ] || { echo "Tensorflow version mismatch, cannot continue. Tensorflow version was updated in pyproject.toml, but not in the Dockerfile. Grab cuDNN and CUDA versions from https://www.tensorflow.org/install/source#gpu and update the Dockerfile near this check."; exit 1; } && \
    echo "Tensorflow version matches pyproject.toml installing dependencies for NVIDIA GPU support (cuDNN $CUDNN_VER, CUDA $CUDA_VER)."


# ---- installing GPUs-specific dependencies ----

# -- installing CUDA Toolkit required for tensorflow-gpu --
RUN apt-get install -yq wget \
    && wget https://developer.download.nvidia.com/compute/cuda/repos/ubuntu2204/x86_64/cuda-keyring_1.0-1_all.deb \
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
ENTRYPOINT ["python"]
