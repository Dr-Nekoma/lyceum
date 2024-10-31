#!/usr/bin/env bash

# Luckily, we've already cached the build from CI
sudo nix build .#dockerImage
sudo docker load < result
sudo docker compose up -d
