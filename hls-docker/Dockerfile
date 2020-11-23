FROM ubuntu:20.04
LABEL maintainer "Kazuki Ishigaki <k-ishigaki@frontier.hokudai.ac.jp>"

RUN apt-get update && apt-get install -y \
    build-essential \
    curl \
    haskell-stack \
	sudo \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

RUN stack upgrade --binary-only

RUN curl -fLo /usr/local/bin/ghcup \
    https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup && \
    chmod +x /usr/local/bin/ghcup && \
    ghcup install ghc 8.6.5 && \
    ghcup set 8.6.5 && \
    ghcup install cabal && \
    ghcup install hls

ENV PATH=/root/.cabal/bin:/root/.ghcup/bin:$PATH

RUN echo "developer ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers.d/developer && \
    chmod u+s `which groupadd` `which useradd` && \
    { \
    echo '#!/bin/sh -e'; \
    echo 'getent group `id -g` || groupadd --gid `id -g` developer'; \
    echo 'getent passwd `id -u` || useradd --uid `id -u` --gid `id -g` --home-dir /root developer'; \
    echo 'sudo find /root -maxdepth 1 | xargs sudo chown `id -u`:`id -g`'; \
    echo 'exec "$@"'; \
    } > /entrypoint && chmod +x /entrypoint
ENTRYPOINT [ "/entrypoint" ]
