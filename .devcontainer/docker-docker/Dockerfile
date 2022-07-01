FROM alpine:3.13 as downloader

RUN apk --no-cache add curl

ARG DOCKER_VERSION=20.10.11
RUN curl https://download.docker.com/linux/static/stable/x86_64/docker-$DOCKER_VERSION.tgz \
    | tar -xz -C /usr/bin --strip=1 docker/docker

FROM alpine:3.13
LABEL maintainer="Kazuki Ishigaki<k-ishigaki@frontier.hokudai.ac.jp>"

RUN apk --no-cache add docker-compose shadow sudo
COPY --from=downloader /usr/bin/docker /usr/bin/docker

# Change the permissions so that any user can run
RUN chown root:root `which docker` && \
    chmod u+s `which docker`

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