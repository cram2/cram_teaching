# WSL Setup

If you work with Windows and want to start from scratch with a new WSL Ubuntu installation, this is how to make Docker, ROS, CRAM, Jupyter and Bullet running on your machine.

## Enable and get Windows Subsystem for Linux

* Enable Hardware Virtualization in your BIOS, somewhere related to CPU settings
* Enable WSL in Windows features
* Update WSL to version 2 with the [msi install file](https://wslstorestorage.blob.core.windows.net/wslblob/wsl_update_x64.msi)
* Download Ubuntu from the store or just execute from Windows Menu (default is 20.04 but others work too)
* Set username and password

You now have a fully functioning Ubuntu installation

## WSL Settings

Enable copy-pasting with Ctrl+Shift+C/V
![enable-copypaste](https://user-images.githubusercontent.com/13121212/197223157-a14faf8d-a546-4cbd-b437-15b82e238c9a.PNG)
Enable scrolling
![enable-scrolling](https://user-images.githubusercontent.com/13121212/197223178-206241eb-4a06-474c-9f6e-523e7eb032a3.PNG)

## Install updates and software Ubuntu

This is a bash terminal. enter the following commands into the terminal to update your Ubuntu distro.

You have a bare bones Ubuntu. Update it like this
```bash
sudo apt update   # fetches potential update references
```
```bash
sudo apt upgrade  # installs latest updates
```
Install the dependencies for Docker
```bash
sudo apt install \
    ca-certificates \
    curl \
    gnupg \
    lsb-release
```
Make directory for the GPG keys
```bash
sudo mkdir -p /etc/apt/keyrings
```
Get the GPG key for Docker
```bash
curl -fsSL https://download.docker.com/linux/debian/gpg | sudo gpg --dearmor -o /etc/apt/keyrings/docker.gpg
```
Make Docker available for automatic updates
```bash
echo \
  "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/debian \
  $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
```
Install Docker Compose
```bash
sudo apt install docker-compose
```

## Set up permissions for Docker

Create the *docker* group
```bash
sudo groupadd docker          # creates the group 'docker'. It may already exist through the installtaion
```
Add yourself (your user) to the *docker* group
```bash
sudo usermod -aG docker $USER # adds the current user to the group 'docker'
```
Update groups settings
```bash
newgrp docker                 # activates the changes in group management.
```

## Automatically start Docker service on boot.

Allow starting Docker from boot. 
```bash
sudo sh -c 'echo \
"# Docker daemon setup \n\
sungkim ALL=(ALL) NOPASSWD: /usr/bin/dockerd" >> /etc/sudoers'
```
Now this will add the execution at the start of the terminal.
```bash
echo \
'
# Start Docker daemon automatically when logging in if not running.
RUNNING=`ps aux | grep dockerd | grep -v grep`
if [ -z "$RUNNING" ]; then
    echo "Starting the docker service requires root rights."
    sudo dockerd > /dev/null 2>&1 &
    disown
fi' >> ~/.bashrc 
```
### Test: Check your setup and permissions to run Docker

Close Ubuntu and start is again. Then execute
```bash
docker run hello-world
```
You should see a message from Docker now. If not, check the setup again.

## Install X-Client

X forwarding transmits a graphical user interface (GUI) from a server to a client. This transmission needs to be set up from two directions. The server needs to send the application, and the client needs to receive it.

This installs the software to allow the server to emit its graphical applications.
```bash
sudo apt install x11-xserver-utils  # installs the utils to allow foreign displays
```
Export the DISPLAY variable
```bash
echo '# Set DISPLAY to the Windows VcXsrv display address
export DISPLAY=$(cat /etc/resolv.conf | grep nameserver | awk '"'"'{print $2}'"'"'):0.0' >> ~/.bashrc
```
Reload your terminal settings
```bash
source ~/.bashrc   # this should not throw an error
```
Check the DISPLAY variable
```bash
echo $DISPLAY  # this should show an IP with 0.0 at the end
```
Before we can enable the server, we need the X-Client

## Install X-Server

VcXsrv is an X-Server, that is able to visualize OpenGL application from remote connections. We use it, because the Docker container is a kind of headless machine that  can only render the robot-simulator internally, but can not visualize without a display to show it. VcXsrv is providing the display such that the Docker application can connect to that display. [This guide](https://medium.com/javarevisited/using-wsl-2-with-x-server-linux-on-windows-a372263533c3) is the foundation for ours.
    
* [Download and install VcXsrv](https://sourceforge.net/projects/vcxsrv/)
* Go to the installed folder, it should be in `C:\Program Files\VcXsrv`
* Right-click the `vcxsrv.exe` to `Create shortcut` to the desktop
* Configure the `VcXsrv.exe - Shortcut`
    * Go to the Desktop and right-click the shortcut, select `Properties` > `Shortcut` > `Target` and append the following to the existing entry:
    * ` :0 -ac -terminate -lesspointer -multiwindow -clipboard -wgl -dpi auto`
    * Then it should look somewhat like this: `"C:\Program Files\VcXsrv\vcxsrv.exe" :0 -ac -terminate -lesspointer -multiwindow -clipboard -wgl -dpi auto`
    * `OK` out of the window
* Execute the shortcut of VcXsrv. It appears that nothing happens. Check the tray icons in the bottom-right corner, there it should show it.

## Adjust Firewall Settings

* Since the display connection is something that Windows' Firewall classifies as dangerous, we need to allow that connection.
* Open Firewall settings with `Windows`-key, 'firewall with advanced', enter
![fw-settings](https://user-images.githubusercontent.com/13121212/190249123-947acf13-17ed-4654-b78f-d0b160ef9303.PNG)

### Test X-Server-Client connection

The Ubuntu WSL is our X-client, that wants to show a graphical application of the X-Server, which is running via VcXsrv in Windows.

Start the VcXsrv - Shortcut with adjusted parameters, as explained above. This is the **X-Server**.

Open Ubuntu and execute the following to open the **X-Client**. It will connect to the IP given by the DISPLAY variable, which is the one of the X-Server.
```bash
xhost +local:docker
```
When it says, that it allow local connections now, we can open an application. First install mesa-utils.
```bash
sudo apt install mesa-utils 
```
Then open an application
```bash
glxgears
```
If that works, the VcXsrv OpenGL forwarding is set up successfully!
* `glxgears` will open up a windows with moving gears. They should run very smoothly. If they don't, somethings wrong with the setup.
* If `glxgears` is stuck for a long time or unable to find the display, check the `DISPLAY` variable in your Ubuntu shell and Firewall settings again.
* If `glxgears` command couldn't be found, do `sudo apt install mesa-utils` to get it.
* If the gears move very slowly, check the parameters for the VcXsrv - Shortcut again.

## Install the Lecture content

Get the lecture with git. Continue with your lecture-specific procedure. Consider the slides for [Robot Programming with Lisp](https://ai.uni-bremen.de/teaching/cs-lisp-ws22) for how to set up your repository, or [follow the Readme here](https://github.com/cram2/cram_teaching/blob/main/README.md#getting-the-lectures-docker-container) for the general setup without assignments.
