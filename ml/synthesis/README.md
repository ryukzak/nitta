# Machine Learning for Synthesis in NITTA

ML can be used to improve speed and quality of synthesis methods in NITTA. It is suggested to use ML model to statistically estimate the numerical target function based on training data gathered from a set of precomputed synthesis trees.

![image](https://user-images.githubusercontent.com/5229130/123153468-5dcae080-d46e-11eb-8867-f9c1944ffae4.png)

A proof of concept has been implemented. Key features:

- communicating with NITTA using its web API
- synthesis tree depth-first traversal to get training data
- features preprocessing, label calculation
- training of a simple ML model (relatively small neural network)
- model evaluation

## Tech stack

- Python 3.7
- Pandas for data processing
- Tensorflow for ML.

## Development Environment

Manual installation of all dependencies can take hours, so using provided Docker container for development is recommended.

![docker container running example](https://user-images.githubusercontent.com/5229130/221037991-dc0c9948-a294-4ed2-b0f3-d5777fee290f.png)

### Setup

Make sure you have [Docker](https://docs.docker.com/get-docker/) installed. Clone the NITTA repo.

> ⚠️ **Important Windows-specific notes**
> - Use Docker Desktop for Windows with WSL 2 backend. Docker Toolbox may work without GPU support, but it wasn't tested.
> - Do everything described above (git clone, docker calls, etc.) in a WSL 2 shell. Decent DX is possible, don't worry. Ubuntu is recommended, tested on 22.04.1 LTS.
> - Checkout NITTA repo to a directory in a **WSL 2 filesystem** (such as `~/dev/nitta`). Do **NOT** use Windows filesystem (i.e. paths like `/mnt/c/Dev/nitta`) – this will severely impact performance and may bring other problems since we're going to bind mount the repo root into the container.
> - Once again, run the Docker commands below **in a WSL 2 shell**.

`cd` into the repo root and run:

```bash
docker build \
	--target development \
	-f ml/synthesis/Dockerfile \
	--build-arg HOST_UID=$(id -u) \
	--build-arg HOST_GID=$(id -g) \
	-t nitta-dev \
	.
```

This will build a development Docker image. What's in the box:
- Stack with GHC, iverilog, hlint, fourmolu, etc.
- Node, NPM, Yarn for web UI development
- Python, a Jupyter Notebook server and dependencies for ML development
- Compiled NITTA Haskell dependencies
- SSH server for remote development with debugging and port forwarding

That was good news. Bad news – it takes considerable time and space to download and compile/install all this. You'll need to do it only once though. Expected build times and image sizes:

| Target | Build time | Image size |
| --- | --- | --- |
| `development` | ~30 min | ~8 GB |
| `development-gpu`* | ~45 min | ~17 GB |
\* more about GPU support [later](#gpu-support-for-ml-models-training-linuxwindows-with-nvidia-gpus-only)

When the building is finished, you can run the container:

```bash
docker run \
	--name=nitta-dev-container \
	-p 8888:8888 \
	-p 2222:22 \
	-v="$(pwd):/app" \
	-v="nitta-devuser-home:/home/devuser" \
	-it \
	nitta-dev
```

This will run the container in interactive mode with a bash shell at your disposal. The repo root is bind-mounted into the container, so you can `ls -lah` and see the files from your host system. Changes made to those will be instantly reflected in your host and vice versa. Jupyter Notebook server and SSH server ports are forwarded so you can access those services. 

You can stop the container with `exit` and start it again with `docker start -ai nitta-dev-container`. Start the container in the beginning of each development session.

Examine the startup output in the console. Try interacting with the environment you've just created:
- run some commands (for example, `htop`, `stack test --fast`)
- open Jupyter Notebook in your host system's browser (see the startup output for the URL, restart the container if you can't find it)
- ssh into the container from another shell session (see the startup output for the command to run)
- try using NITTA built in the container:
    - build the web UI: `stack exec nitta-api-gen && cd web && yarn install && yarn build && cd ..`
    - run the web UI: `stack exec nitta -- -p=8080 examples/counter.lua`
    - forward the port to your host system with additional `-fNL 8080:localhost:8080` flags to ssh
    - open the web UI in your host system's browser
- see what's next in the Usage section below

### Usage

The container is expected to be:
- long-lived:
    - it's suggested to start/stop the container instead of recreating
    - attach/ssh into it to change its state interactively like you would do with a VM
    - state is preserved between container restarts
    - state in some important directories (`~` and `/app`) is preserved between container recreations (be careful, can break stuff!)
- always running when you're working on NITTA:
    - use remote development over SSH for a normal development flow
    - run container in background if you wish:
        - replace `-it` with `-itd` in the `docker run`
        - `docker start` the container without `-ai` if it's already created

The user everything is running under is called `devuser`. Its UID/GID is matching your host user to avoid file permission issues when bind-mounting the repo root into the container. You can work directly on the repo files and changes will be instantly reflected in the container.

User's home directory is also persisted in a Docker named volume `nitta-devuser-home`. This allows to keep the important part of container's state between container recreations (e.g. IDE settings, stack cache, etc.)

#### Remote development over SSH

SSH server running in the container allows to use:
- VS Code with [Remote - SSH](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-ssh) extension (Recommended)
- [JetBrains Gateway](https://www.jetbrains.com/remote-development/gateway/) for full-fledged remote development in JetBrains IDEs
- Jupyter Notebook server as an interactive Python REPL for ML development
- text editors like Vim, Emacs, etc.
- any other IDE that supports remote development over SSH 

The key to access the SSH server is generated during the first container startup and is stored in your host filesystem in the repo (via bind mount). See the container startup output for the path to the key file and more details.

### GPU support for ML models training (Linux/Windows with NVIDIA GPUs only)

If you want to have your GPU available in Docker, use:
- `--target=development-gpu` with `docker build`
- `--gpus=all` argument with `docker run`

⚠️ Check platform-specific notes below.

Full examples:

```bash
docker build \
	--target development-gpu \
	-f ml/synthesis/Dockerfile \
	--build-arg HOST_UID=$(id -u) \
	--build-arg HOST_GID=$(id -g) \
	-t nitta-dev \
	.

docker run \
	--name=nitta-dev-container \
	--gpus all \
	-p 8888:8888 \
	-p 2222:22 \
	-v="$(pwd):/app" \
	-v="nitta-devuser-home:/home/devuser" \
	-it \
	nitta-dev
```

#### Linux

1. Make sure you're running a recent version of NVIDIA GPU driver (495+).
1. Install [NVIDIA Container Toolkit](https://docs.nvidia.com/datacenter/cloud-native/container-toolkit/install-guide.html#docker) prior to running the container, it's required.
1. Run the Docker commands above **in the repo root**.

Not yet tested on Linux, but should work.

#### Windows
Everything should work out of the box, run the Docker commands above **inside WSL 2 and in the repo root**.

Make sure you're running recent versions of Windows (11 or 10 21H2+), NVIDIA GPU driver (495+) and Docker Desktop with WSL 2 backend.

If something's wrong, welcome to the beautiful world of  ✨ troubleshooting this Danse Macabre ✨

1. Ensure that the version of your NVIDIA GPU driver is recent (495+) by running `nvidia-smi` in cmd. The `nvidia-smi` tool should be available at `C:\Windows\System32\nvidia-smi.exe` by default if the driver is installed.
1. Check that your Docker Engine itself (regardless of GPU paravirtualization) is fine: `docker run hello-world` should work.
1. **GPU driver should be installed on your host machine only**, it should not be necessary to install it in any WSL 2 distro or Docker containers.
1. Ensure that you have a GPU-enabled WSL 2 ([more info](https://learn.microsoft.com/en-us/windows/ai/directml/gpu-cuda-in-wsl)). Run `nvidia-smi` inside your regular WSL 2 distro (Ubuntu recommended). Output should give you the correct info about your GPU (this tool should also be available out of the box at `/usr/lib/wsl/lib/nvidia-smi` for Ubuntu 22.04.1 LTS just because your **host Windows machine** has a recent NVIDIA driver). 
1. Check that [The NVIDIA Container Runtime Hook](https://docs.nvidia.com/datacenter/cloud-native/container-toolkit/arch-overview.html) in your Docker Desktop is working fine and the GPU (with nvidia-smi) is also available in your Docker containers: `docker run --gpus=all ubuntu nvidia-smi` should give you the same correct info about your GPU. This works because NVIDIA GPUs in Docker Desktop for Windows in WSL 2 environment [should be supported since 3.1.0](https://docs.docker.com/desktop/windows/wsl/#gpu-support). Those docs are outdated as of 2023.01, since support has been merged into stable Windows/driver branches though.
1. More additional info can also be found in the [Tensorflow docs](https://www.tensorflow.org/install/docker#gpu_support).


