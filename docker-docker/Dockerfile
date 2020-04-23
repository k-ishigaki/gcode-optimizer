FROM alpine as builder

# container name
ARG CONTAINER_NAME
RUN test -n "${CONTAINER_NAME}" || (echo "CONTAINER_NAME not set" 1>&2 && false)
# external container commands
ARG CONTAINER_COMMANDS='k-ishigaki/git:git'

RUN apk --no-cache add bash curl
RUN curl -fsSL "https://get.docker.com/builds/`uname -s`/`uname -m`/docker-latest.tgz" \
	| tar -xz -C /usr/local/bin --strip=1 docker/docker && \
	chmod u+s /usr/local/bin/docker
COPY ./generate_docker_cmd /generate_docker_cmd
RUN chmod +x ./generate_docker_cmd && \
    ./generate_docker_cmd ${CONTAINER_NAME} ${CONTAINER_COMMANDS}

FROM busybox
LABEL maintainer="Kazuki Ishigaki<k-ishigaki@frontier.hokudai.ac.jp>"

COPY --from=builder /usr/local/bin/* /usr/local/bin/
