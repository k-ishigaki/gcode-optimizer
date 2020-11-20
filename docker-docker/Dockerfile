FROM alpine:3.11 as downloader

RUN apk --no-cache add curl

RUN curl https://download.docker.com/linux/static/stable/x86_64/docker-19.03.9.tgz \
    | tar -xz -C /usr/bin --strip=1 docker/docker && \
    # Change the permissions so that any user can run
    chown root:root `which docker` && \
    chmod u+s `which docker`

FROM busybox
LABEL maintainer="Kazuki Ishigaki<k-ishigaki@frontier.hokudai.ac.jp>"

COPY --from=downloader /usr/bin/docker /usr/bin/docker
