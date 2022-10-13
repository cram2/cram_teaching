# cram-teaching
Lectures on CRAM in Jupyter Notebooks via Docker

We use Docker to provide the software to you, such that you can interact with it on your host machine. Within these Docker containers we provide Jupyter Notebook content, which is a web-platform for the Browser. Jupyter Notebook is a combination of a Markdown document and code blocks, which allows the user to execute code in the browser and read the explanation of it all in one place.

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
