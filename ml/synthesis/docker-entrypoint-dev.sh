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
# create authorized_keys if the file doesn't exist
[ ! -f ~/.ssh/authorized_keys ] && touch ~/.ssh/authorized_keys
# append the key to authorized_keys if it's not already there
grep -qxF "$(cat "$ssh_key_location.pub")" ~/.ssh/authorized_keys || cat "$ssh_key_location.pub" >> ~/.ssh/authorized_keys

# make profile settings available in this script
[ -f ~/.profile ] && . ~/.profile

# start a ssh server as a screen daemon
screen -dmS sshd sudo -s /usr/sbin/sshd -D
echo "SSH into this container for remote development in IDEs: ssh -i $ssh_key_location -p 31032 $(whoami)@localhost"

# print info about GPUs available to Tensorflow
python -c "import shutup, os; shutup.please(); os.environ['TF_CPP_MIN_LOG_LEVEL'] = '3'; import tensorflow as tf; gpu_count = len(tf.config.list_physical_devices('GPU')); print(f'Num of GPUs available to Tensorflow: {gpu_count}')"

# remove defunct screens
screen -wipe > /dev/null

echo "Available screens (attach with screen -r <name>):"
screen -ls | sed '1d;$d'


# fallback to shell for interactivity
TERM=xterm-256color /bin/bash
