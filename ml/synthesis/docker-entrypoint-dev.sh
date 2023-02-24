#!/bin/bash

# get/create a key to SSH into the container and authorize it
ssh_key_location=ml/synthesis/.dev/container_ssh_key
if [ ! -f "$ssh_key_location" ]; then
    echo "Generating a new SSH key for remote access to this container..."
    mkdir -p "$(dirname "$ssh_key_location")"
    ssh-keygen -q -t ed25519 -f "$ssh_key_location" -N ""
    chmod 600 "$ssh_key_location"
    echo "New key can be found at <repo_root>/$ssh_key_location, it will be automatically authorized in the container on every launch."
fi
mkdir -p "$HOME/.ssh"
cat "$ssh_key_location.pub" >> "/home/$(whoami)/.ssh/authorized_keys"  # $HOME is not set in this script

jupyter_port=${JUPYTER_PORT:-8888} 
jupyter_token_filepath=ml/synthesis/.dev/jupyter_token
if [ ! -f "$jupyter_token_filepath" ]; then
    jupyter_token=$(python3 -c "import secrets; print(secrets.token_hex(24))")
    echo "$jupyter_token" > "$jupyter_token_filepath"
else
    jupyter_token=$(cat "$jupyter_token_filepath")
fi

# start a ssh server as a screen daemon
screen -dmS sshd sudo -s /usr/sbin/sshd -D
echo "SSH into this container if needed for remote debugging in IDEs: ssh -i $ssh_key_location -p 2222 -o "UserKnownHostsFile=/dev/null" -o "StrictHostKeyChecking=no" $(whoami)@localhost"

# start a jupyter notebook server as a screen daemon
screen -dmS jupyter-notebook jupyter notebook --port $jupyter_port --NotebookApp.token="$jupyter_token" --ip 0.0.0.0
echo "Jupyter Notebook should be available at: http://localhost:$jupyter_port/?token=$jupyter_token"

# print info about GPUs available to Tensorflow
python3 -c "import shutup, os; shutup.please(); os.environ['TF_CPP_MIN_LOG_LEVEL'] = '3'; import tensorflow as tf; gpu_count = len(tf.config.list_physical_devices('GPU')); print(f'Num of GPUs available to Tensorflow: {gpu_count}')"

screen -wipe > /dev/null

echo "Available screens (attach with screen -r <name>):"
screen -ls | sed '1d;$d'

# remove the sudo tutorial on startup
touch ~/.sudo_as_admin_successful

source ~/.profile

# fallback to shell for interactivity
TERM=xterm-256color /bin/bash
