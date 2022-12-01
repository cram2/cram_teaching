#!/bin/bash

## Set this script as entrypoint for your Docker image in your docker-compose.yml
##     entrypoint: ./lectures/init.sh
##
## Everything in here is executed on boot of the Docker container.
## When this script ends, the container dies.

source /home/workspace/ros/devel/setup.bash

# Uncomment this when your workspace is built
# echo "source /home/lectures/robot_programming_with_lisp/06_turtle_party/ros_ws/devel/setup.bash" >> ~/.bashrc
# jupyter-lab --allow-root --no-browser --port 8888 --ip=0.0.0.0 &

echo "Booting in headless mode, starting roscore."
echo "docker exec -it cram_container /bin/bash  # to attach to container"
roscore
echo "ROSCORE already running. Going to sleep..."
sleep infinity
