# cram-teaching - quick setup for Ubuntu 20.04

With this guide you will install the docker-compose plugin to run JupyterNotebook with CRAM.

## Virtualization

Enable CPU virtualization in your BIOS to run virtual machines (VMs). If VMs are already enabled, there's nothing to do here.

Check if VMs are enabled with kvm-ok

```bash
sudo apt-get install cpu-checker
```
```bash
kvm-ok
```

## Set up the Docker repository for aptitude

[Reference Manual](https://docs.docker.com/engine/install/ubuntu/#set-up-the-repository)

1. Update the apt package index and install packages to allow apt to use a repository over HTTPS:
```bash
sudo apt-get update
```
```bash
sudo apt-get install ca-certificates curl gnupg
```
2. Add Docker’s official GPG key:
```bash
sudo install -m 0755 -d /etc/apt/keyrings
```
```bash
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /etc/apt/keyrings/docker.gpg
```
```bash
sudo chmod a+r /etc/apt/keyrings/docker.gpg
```
3. Use the following command to set up the repository:
```bash
echo \
  "deb [arch="$(dpkg --print-architecture)" signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/ubuntu \
  "$(. /etc/os-release && echo "$VERSION_CODENAME")" stable" | \
  sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
```
## Install docker
```bash
sudo apt-get install docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin
```
Verify that Docker Compose is installed correctly by checking the version.
```bash
docker compose version
```
## Set user permissions

Put the user in the docker group, so we don't need to add sudo to every docker command.
```
sudo groupadd docker
```
```bash
sudo usermod -aG docker $USER
```
```bash
newgrp docker
```
Logout and login to make this change permanent.

## Install Nvidia Drivers

Do you have an NVIDIA GPU in your machine? Make sure you have the driver installed.

<details>

In Software & Updates > Additional Drivers > choose the latest NVIDIA driver.

### Install Docker NVIDIA

[Reference Manual](https://github.com/HoangGiang93/mujoco_sim_docker/blob/TiagoInApartment/setup_rviz.bash)

```bash
curl -s -L https://nvidia.github.io/nvidia-docker/gpgkey | sudo apt-key add -
```
```bash
distribution=$(. /etc/os-release;echo $ID$VERSION_ID)
```
```bash
curl -s -L https://nvidia.github.io/nvidia-docker/$distribution/nvidia-docker.list | sudo tee /etc/apt/sources.list.d/nvidia-docker.list
```
```bash
sudo apt-get update
```
```bash
sudo apt-get install -y nvidia-docker2
```
```bash
sudo pkill -SIGHUP dockerd
```
```bash
sudo systemctl daemon-reload
```
```bash
sudo systemctl restart docker
```
  
</details>

## Start the docker container

Run
```bash
./start_eurobin.sh
```

The script will enable x-forwarding for docker and perform `docker compose up cram-eurobin`, which will pull the image from DockerHub and launch it.

In your Terminal there should be a link. Paste it to your browser to open the Jupyter Notebook for CRAM.
