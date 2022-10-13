# cram-teaching
Lectures on CRAM in Jupyter Notebooks via Docker

We use Docker to provide the software to you, such that you can interact with it on your host machine. Within these Docker containers we provide Jupyter Notebook content, which is a web-platform for the Browser. Jupyter Notebook is a combination of a Markdown document and code blocks, which allows the user to execute code in the browser and read the explanation of it all in one place.

## How do you want to do this?

Depending on your system, your choice may be limited. but we got you covered. These are the options we offer:

* Option 1: Docker Compose -- Linux native (tested with Debian)
* Option 2: WSL2 tar-ball import with pre-installed Docker -- Windows + WSL2 + VcXsrv (tested with Win10)
* Option 3: VirtualBox image -- fallback for any OS whatsoever

## Enable Hardware-Virtualization
<details>

Hardware Virtualization is a setting for your CPU, that enables it to run virtual operating systems on your host machine. Depending on your host system, the lectures of the Fall School run in Docker, the Windows Subsystem for Linux or a VirtualBox VM. All of these option use virtualization of another operating system underneath your existing one. We offer our software like that, such that we can ensure that it runs on a manifold of different systems.

VT-x for intel, AMD-V for AMD chips

https://www.virtualmetric.com/blog/how-to-enable-hardware-virtualization

### Get into your BIOS
On Linux, shutdown the machine and boot after that (rebooting sometimes doesn't load the BIOS with fast-boot settings)

On Windows, go to the Windows menu (bottom left) > Power > hold the Shift-key while clicking Restart. It will reboot into a blue UI. Choose Troubleshoot, Change behaviour on boot, Restart. This prevents Windows from fast-booting.

When the mainboard prompts, press one of the suggested buttons (F2, F9, F12, DEL, etc.), or do so while it boots like once a second. Go to 'Advanced' and search for Hardware Virtualization, VT-x or AMD-V, something like that. The menus are different for every mainboard. Save and boot.

On Windows it offers some options for recovery, which is from the chosen troubleshoot before. Just ESC out of it.

</details>

## Option 1: Docker Compose Setup (recommended for Linux)

<details>

For Linux users, the `docker-compose` package includes all necessary functionality. The lecture's software is build with docker-compose.yml files, which enable easy maintainance of collaborating Docker images. Docker on Windows relies on Docker Desktop, which needs a Linux kernel to run Containers, and to visualize X-Applications it also needs an X-Server. VcXsrv does work and only needs minimal configuration, while xMing can't handle OpenGL/Glut rendering as well. But the setup requires lots of tweaking the Firewall and getting things connected, so instead we chose to prepare a WSL image and run Docker from there (see Option 2). For MacOS we weren't able to test X-Forwarding, and without visualization of the simulator, the lecture for CRAM can't operate at all.

### Linux

Install utility software before installing Docker 
```bash
sudo apt update
sudo apt install \
    ca-certificates \
    curl \
    gnupg \
    lsb-release
```
Get keyring and Docker's package references
```bash
sudo mkdir -p /etc/apt/keyrings
curl -fsSL https://download.docker.com/linux/debian/gpg | sudo gpg --dearmor -o /etc/apt/keyrings/docker.gpg
echo \
  "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/debian \
  $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
```
Install docker-compose
```bash
sudo apt install docker-compose
```
#### Postinstall ([troubleshoot here](https://docs.docker.com/engine/install/linux-postinstall/))
Give your user permission to use Docker by adding it to the 'docker' group
```bash
sudo groupadd docker          # creates the group 'docker'. It may already exist through the installtaion
sudo usermod -aG docker $USER # adds the current user to the group 'docker'
newgrp docker                 # activates the changes in group management.
```
Start the docker daemon
```bash
sudo dockerd # makes sure the daemon runs. They may already be running though.
```
Test installation and postinstall
```bash
docker run hello-world
```
Allow docker to open x-Applications, like the robot simulator
```
sudo apt install x11-xserver-utils # installs the utils to allow foreign displays
xhost +local:docker                # allows x-forwarding for the 'docker' group
```
#### Troubleshoot
    
We're doing x-forwarding with xhost, which hasn't been tested with the Wayland display manager, but with x11. Check your display manager like this:
```bash
loginctl show-session $(awk '/tty/ {print $1}' <(loginctl)) -p Type | awk -F= '{print $2}'
```
If you want to switch to gdm3 (x11), [follow this guide](https://linuxconfig.org/how-to-enable-disable-wayland-on-ubuntu-20-04-desktop).
    
When xhost can't open the Display, find it with
```bash
ps -u $(id -u) -o pid= \
    | xargs -I PID -r cat /proc/PID/environ 2> /dev/null \
    | tr '\0' '\n' \
    | grep ^DISPLAY=: \
    | sort -u
```
and set it with
```bash
export DISPLAY=:0 # or :1. Put this line in your ~/.bashrc file
```
The Docker container inherits the `DISPLAY` variable from your host system.

When `docker run hello-world` doesn't work because of missing permissions, check
```bash
groups
```
and see if `docker` is listed. If it's not, check the *Linux Posinstall* above. If it is, re-login or reboot you machine to reset user permissions.

If `docker-compose up` (see below, when starting a lecture) complains about issues about services or processes, run
```bash
sudo dockerd
```

While `dockerd` is supposed to start the Docker services, you can also check and restart them explicitly:
```bash
systemctl status docker.service
systemctl status docker.socket

sudo systemctl restart docker.service
sudo systemctl restart docker.socket
```
If systemctl doesn't work for your setup, e.g. when it runs on systemd, you can run `dockerd` automatically on boot with [this procedure](https://medium.com/geekculture/run-docker-in-windows-10-11-wsl-without-docker-desktop-a2a7eb90556d).

Ubuntu 22.04 (Jammy Jellyfish) has no current release of Docker Compose, but can still be installed. You need to adjust the `docker-compose.yml` though, and delete the entries for port forwarding, which 22.04 can't handle with the setting `network_mode: host`. These lines can be removed:
```yaml
...
    ports:
      - "8888:8888"
    expose:
      - "8888"
...
```

If the docker container is running, but there's still something wrong, you can open a bash in the container and debug yourself, e.g. check the value of the `DISPLAY` variable.
```
docker exec -it <container name> /bin/bash  # Opens a bash in the container. Check 'docker container list' to get the name
$ echo $DISPLAY                             # executed in the container's bash, you can check the DISPLAY variable's value.
```

If it still doesn't work, reinstall docker. First remove the current installation
```bash
sudo apt prune docker-compose
```
and start from the top. `docker-compose` installs all the other required docker packages to run the lecture.

</details>
