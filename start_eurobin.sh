#!/bin/bash
# Allows x-forwarding and starts docker

xhost +local:docker

docker compose up cram-eurobin

xhost -local:docker
