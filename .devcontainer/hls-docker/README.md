# hls-docker

A haskell-language-server excutable via Docker.  
It contains GHC, Stack, Cabal.

## Features

 * Changeable uid/gid when running container

## Requirements

 * Docker

## Build

```Shell
git clone https://github.com/k-ishigaki/hls-docker.git
cd hls-docker
docker build -t k-ishigaki/hls .
```

## Run

```Shell
docker run --rm -it -v $(pwd):/root/workspace -w /root/workspace -u $(id -u):$(id -g) k-ishigaki/hls haskell-language-server-wrapper ...
```
