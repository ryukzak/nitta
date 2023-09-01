# Machine Learning for Synthesis in NITTA

ML can be used to improve speed and quality of synthesis methods in NITTA. It is suggested to use ML model to statistically estimate the numerical target function based on training data gathered from a set of precomputed synthesis trees.

![image](https://user-images.githubusercontent.com/5229130/123153468-5dcae080-d46e-11eb-8867-f9c1944ffae4.png)

A proof of concept has been implemented. Key features:

- communicating with NITTA using its web API
- synthesis tree iterative sampling to get training data
- features preprocessing, label calculation
- training of a baseline ML model (relatively small neural network)
- full integration of ML node scoring into "native" NITTA synthesis process (includes ML backend server)
- synthesis performance evaluation and comparison

## Tech stack

- Python 3.11 with Poetry-managed dependencies
- Pandas for data processing
- Tensorflow for ML
- FastAPI on uvicorn for ML backend server
- Black, mypy, ruff and vulture for code formatting/linting
- Pytest for testing

## Development Environment

Manual installation of all dependencies can take hours, so it's recommended to use Docker to create a development environment faster and easier.

![docker container running example](https://github.com/ryukzak/nitta/assets/5229130/edc1dd0f-6fc4-4136-b4af-ff56dd931dfb)

### Setup

#### Preparing the system

Make sure you have [Docker](https://docs.docker.com/get-docker/) installed. Clone the NITTA repo.

> ⚠️ **Important Windows-specific notes**
>
> - Use **Docker Desktop for Windows with WSL 2 backend**. Docker Toolbox may work without GPU support, but it wasn't tested.
> - Do everything described above (git clone, docker calls, etc.) **in a WSL 2 shell**. Ubuntu is recommended, tested on 22.04.1 LTS.
> - Checkout NITTA repo to a directory in a **WSL 2 filesystem** (such as `~/dev/nitta`). Do **NOT** use Windows filesystem (i.e. paths like `/mnt/c/Dev/nitta`) – this will severely impact performance and may bring other problems since we're going to bind mount the repo root into the container.

#### Building the image

`cd` into the cloned repo root and run:

```bash
# Windows-WSL2 / Linux
docker build \
 --target development \
 -f ml/synthesis/Dockerfile \
 --build-arg HOST_UID=$(id -u) \
 --build-arg HOST_GID=$(id -g) \
 -t nitta-dev \
 .

# MacOS (no HOST_UID/HOST_GID arguments, osxfs handles bind mount permissions)
docker build \
 --target development \
 -f ml/synthesis/Dockerfile \
 -t nitta-dev \
 .
```

⚠ If you have an NVIDIA GPU and need to use it in the container (ML model training), adjust the build arguments [as described here](#gpu-support-for-ml-models-training-linuxwindows-with-nvidia-gpus-only).

This will build a development Docker image. What's in the box:

- GHCup with Stack, GHC, HLS, iverilog, hlint, fourmolu, etc.
- Node, npm, yarn for web UI development
- Python 3.11 with pyenv, poetry, pytest, black, mypy, ruff, vulture, etc.
- Precompiled Haskell NITTA dependencies
- JS/TS dependencies of the web UI
- Python dependencies for the ML-based synthesis
- SSH server for remote development in IDEs

That was good news. Bad news – it takes considerable time and space to download and compile/install all this. Don't worry: you'll need to do it only once and the process is automated. Expected build times and image sizes:

| Target            | Build time | Image size |
| ----------------- | ---------- | ---------- |
| `development`     | ~35 min    | ~16 GB     |
| `development-gpu` | ~45 min    | ~25 GB     |

#### Running the container

When the building is finished, you can run the container. **This may take a while.**

```bash
docker run \
 --name=nitta-dev-container \
 -p 31032:22 \
 -v="$(pwd):/app" \
 -v="nitta-devuser-home:/home/devuser" \
 -it \
 nitta-dev
```

This will run it in interactive mode with a bash shell at your disposal. Let's break the command down:

- `--name=nitta-dev-container`: the container is named for convenience, so you can refer to it later
- `-p 31032:22`: SSH server port is forwarded to your host system (adjustable, but 31032 is assumed everywhere)
- `-v="$(pwd):/app"`: the repo root is bind-mounted into the container ([more info below](#technical-details-about-used-bind-mounts-and-volumes))
- `-v="nitta-devuser-home:/home/devuser"`: the `devsuser` home directory is preserved as a named volume ([more info below](#technical-details-about-used-bind-mounts-and-volumes))
- `-it`: runs the container in interactive mode with current terminal attached, so you can easily see the output and play with it for a while (it's suggested to run it in a detached state later)
- `nitta-dev`: name of the image to run (set in the `docker build` command above)

Thanks to the bind mount, you can `ll` in the `/app` workdir inside the container and see the NITTA repository contents from your host system. Changes made inside the `/app` dir will be instantly reflected in the container and vice versa.

The user everything is running under is called `devuser`. You'll be always logged in as this user.

#### Interacting with the container

Examine the container startup output in the console. Try using the environment you've just created:

- ssh into the container from another shell session (see the startup output for the command to execute it in the repo root)
- run some commands (for example, `htop`, `stack test --fast`)
- try using NITTA in the container:
    - build it: `stack build --copy-bins`
    - build the web UI: `nitta-api-gen && yarn --cwd=web install && yarn --cwd=web build`
    - run the web UI: `nitta -v -p=8080 examples/counter.lua`
    - forward the port to your host system with additional `-NL 8080:localhost:8080` flags to `ssh`
    - open the web UI in your host system's browser
- prepare to see what's next in the "How to use" section below

#### Stopping and restarting the container

You can stop the attached container with `exit` or `Ctrl+D` / `⌘D` and start it again in a detached state with `docker start nitta-dev-container`.

Start the container in the beginning of each development session.

If you need a shell access, depending on a state of things, you can use:

- `ssh` (recommended, you can also add a config to do just `ssh ndc`, it'll be covered later)
- Terminal in a Docker Desktop client (if you use Docker Desktop)
- `docker attach` (when a container is running)
- `docker start -ai` (when a container is stopped and you need to start it briefly, it will stop once you end this session)
- `docker run -it` (when a container isn't created yet, it will also be stopped at the end of this session)

### How to use

The container is expected to be:

- long-lived:
    - it's suggested to start/stop the container instead of recreating
    - attach/ssh into it to change its state interactively like you would do with a VM
    - state is preserved between container restarts
    - state in some important directories (`/home/devuser` and `/app`) is preserved even between container recreations to keep things safe and/or speed them up (shouldn't be necessary in most cases as you shouldn't recreate the container)
- always running when you're working on NITTA:
    - use SSH remote development in IDEs for a decent development experience
    - run the container in background:
        - `docker start nitta-dev-container`
        - if you use Docker Desktop and chose to start the Docker Engine manually, you can use the Docker Desktop GUI client to start the container right after the engine

The steps below should give you an idea of what to do next to finally start writing and testing the code.

#### Remote development over SSH

Thanks to the SSH server built into the container, a developer can use:

- Visual Studio Code with [Remote - SSH](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-ssh) extension
- [JetBrains Gateway](https://www.jetbrains.com/remote-development/gateway/) for remote development in JetBrains IDEs (arguably unusable as of Q1 2023, too much bugs)
- text editors like Vim, Emacs, etc.
- any other IDE that supports remote development over SSH

The recommended, battle-tested way is to open [a remote workspace over SSH](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-ssh) in your local VS Code client:

![VS Code remote dev environment](https://github.com/ryukzak/nitta/assets/5229130/ec532df3-267f-4aa8-96aa-2f2938560cab)

The experience is pretty seamless: required ports are forwarded to the host system automatically, remote debugging and terminal sessions work flawlessly. A preconfigured workspace with needed extensions supporting used tools for formatting/linting/testing/etc. is included in the repo. Solutions for all major problems are already implemented and added to the Docker image.

At the moment of writing (2023), this is arguably the fastest and most convenient way to start developing NITTA in the container. The major part of ML synthesis was developed using this approach.

To get started with it, install or open [VS Code](https://code.visualstudio.com/) and get the [extension](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-ssh). Then follow the [official VS Code docs](https://code.visualstudio.com/docs/remote/ssh), but keep an eye on this text too: it will be required to create an SSH config, see below for some container-specific info on that.

After you connect to the remote:

- go to "File > Open Workspace from File",
- open the `/app/.vscode/nitta.code-workspace`,
- trust it,
- install all recommended extensions,
- check that the right Python interpreter is set in all subroots:
    - do the `Select Interpeter` via `Ctrl+Shift+P` / `⇧⌘P`,
    - select the interpreter at workspace level,
    - choose "⚙️ Use Python from `python.defaultInterpreterPath` setting",
- [configure Git](#git-configuration),
- reload the window,
- check that extensions work (type resolvers, autoformatters, etc., in haskell/python/tsx).

For the basics, that's it! You should be good to go.

#### Creating an SSH config

To conveniently reach the container in IDEs or from shell (like `ssh ndc`) you should create an ssh config for it.

First, find or create your `~/.ssh/config` file.

> **Windows-specific notes**
>
> The OpenSSH for Windows client ([make sure you have it](https://learn.microsoft.com/en-us/windows-server/administration/openssh/openssh_install_firstuse?tabs=powershell)) takes config from `%USERPROFILE%/.ssh/config`.
>
> **VS Code-specific notes**
>
> It's easier to manage the config directly from VS Code. Press `Ctrl+Shift+P` / `⇧⌘P` and type `remote-ssh` to see available commands provided by the extension. "Open SSH Configuration File" is the one you need.

Add this to the config:

```ssh-config
Host ndc
  HostName localhost
  User devuser
  Port 31032
  IdentityFile "<path to the SSH key>"
```

A key to access the container is generated during the first container startup and is stored in your host filesystem in the repo. See the container startup output for the path to the key file.

Set the correct path to the key in the config and that's it. Test with `ssh ndc` in the terminal or try to connect to the host in VS Code.

> **Windows-specific notes**
>
> The needed key was placed into the WSL 2 filesystem, not Windows'. Although it's possible to provide a direct path to the key anyway (it will look like `\\wsl$\...\.dev\container_ssh_key`), you will face the ["UNPROTECTED PRIVATE KEY FILE" error](https://superuser.com/q/1296024/1136773). OpenSSH doesn't like the "too open" **Windows file permissions** (not WSL's) set on the private key file by default. There seems to be [no practical way to disable this check](https://serverfault.com/a/990019). `chmod` in WSL won't help: they're `-rw-------` already.
>
> Suggested solution: copy the key to the Windows filesystem, update its permissions and provide a path to the copy. You'll need to do this once.
>
> Here's a script that automates this for you ([kudos](https://superuser.com/a/1329702/1136773)). Adjust the vars (open `\\wsl$\` in Explorer to find the `$Source` path) and run those lines in a PowerShell session **with admin rights**.
>
> ```powershell
> # Set the vars:
> $Source = "\\wsl$\<distro>\<repo_root>\ml\synthesis\.dev\container_ssh_key"
> $Key = "$env:UserProfile\.ssh\nitta_dev_container_ssh_key"
>
> cp $Source $Key
> cp "$Source.pub" "$Key.pub"
>
> # Remove inheritance:
> Icacls $Key /c /t /Inheritance:d
>
> # Set ownership to owner:
> TakeOwn /F $Key
> Icacls $Key /c /t /Grant:r ${env:UserName}:F
>
> # Remove all users' permissions, except for owner's:
> Icacls $Key /c /t /Remove:g Administrator "Authenticated Users" BUILTIN\Administrators BUILTIN Everyone System Users
>
> # Verify:
> Icacls $Key
>
> echo "Full path to the key: $Key"
> ```
>
> Now you can put the key path in the config, save it and `ssh ndc` right from PowerShell to check if it works.

#### Git configuration

You'll need to [configure Git](https://docs.github.com/en/get-started/getting-started-with-git/setting-your-username-in-git) inside the container to use it:

```bash
git config --global user.name "Mona Lisa"
git config --global user.email "lisa@example.com"
```

#### Technical details about used bind mounts and volumes

On Windows-WSL2 and Linux the UID/GID of `devuser` is matching your host UID/GID to avoid file permission issues when bind-mounting the repo root into the container.

User's home directory (`/home/devuser`) is persisted in a Docker named volume `nitta-devuser-home`. This keeps intact the important part of container's state (e.g. IDE settings, stack cache, etc.) even if something bad happens (like you accidently delete the container or it gets `docker system prune`d). It saves the data between container recreations. Be aware of it, since it can not only help, but also break stuff.

#### GPG commit signing

If you sign your commits with a GPG key, you'll likely need to [generate](https://docs.github.com/en/authentication/managing-commit-signature-verification/generating-a-new-gpg-key) or [import](https://gist.github.com/angela-d/8b27670bac26e4bf7c431715fef5cc51) a new key and [tell git about it](https://docs.github.com/en/authentication/managing-commit-signature-verification/telling-git-about-your-signing-key).

There's another problem: at the time of writing IDEs seem to lack support for accepting a GPG key passphrase over SSH (well, @iburakov couldn't make it work), so a workaround has been implemented. Before doing a commit, run `pass` in a container terminal, enter your passphrase there, and `gpg-agent` will cache it for 3 hours (by default, configurable). Now commit will succeed in IDE without problems.

#### Exposing the SSH port for remote access

You can also set up the development container on your remote or home server and expose the SSH port (via VPN or the Internet, but keep security in mind) to connect to it, for example, from your laptop to work remotely. Everything will look like it's local, but, if you're on the go, it may significantly reduce battery usage, speed up heavy computations and increase download/upload speed if what you have locally is relatively slow.

#### GPU support for ML models training (Linux/Windows with NVIDIA GPUs only)

If you want to have your GPU available in Docker:

- `docker build` for `--target=development-gpu` instead of `development`
- `docker run` with `--gpus=all` argument present
- ⚠️ Check platform-specific notes below.

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
 --gpus=all \
 -p 31032:22 \
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

Everything is battle-tested and should work out of the box given you're running recent versions of Windows (11 or 10 21H2+), NVIDIA GPU driver (495+) and Docker Desktop with WSL 2 backend.

Run the Docker commands above **inside WSL 2 and in the repo root**.

If (suddenly) something's wrong, those suggestions may help you in the beautiful world of ✨ troubleshooting this Danse Macabre ✨

1. Ensure that the version of your NVIDIA GPU driver is recent (495+) by running `nvidia-smi` in cmd. The `nvidia-smi` tool should be available at `C:\Windows\System32\nvidia-smi.exe` by default if the driver is installed.
1. Check that your Docker Engine itself (regardless of GPU paravirtualization) is fine: `docker run hello-world` should work.
1. **GPU driver should be installed on your host machine only**, it should not be necessary to install it in any WSL 2 distro or Docker containers.
1. Ensure that you have a GPU-enabled WSL 2 ([more info](https://learn.microsoft.com/en-us/windows/ai/directml/gpu-cuda-in-wsl)). Run `nvidia-smi` inside your regular WSL 2 distro (Ubuntu recommended). Output should give you the correct info about your GPU (this tool should also be available out of the box at `/usr/lib/wsl/lib/nvidia-smi` for Ubuntu 22.04.1 LTS just because your **host Windows machine** has a recent NVIDIA driver).
1. Check that [The NVIDIA Container Runtime Hook](https://docs.nvidia.com/datacenter/cloud-native/container-toolkit/arch-overview.html) in your Docker Desktop is working fine and the GPU (with nvidia-smi) is also available in your Docker containers: `docker run --gpus=all ubuntu nvidia-smi` should give you the same correct info about your GPU. This works because NVIDIA GPUs in Docker Desktop for Windows in WSL 2 environment [should be supported since 3.1.0](https://docs.docker.com/desktop/windows/wsl/#gpu-support). Those docs are outdated as of 2023.01, since support has been merged into stable Windows/driver branches though.
1. More related info can also be found in the [Tensorflow docs](https://www.tensorflow.org/install/docker#gpu_support), it's a good idea to check them too.
