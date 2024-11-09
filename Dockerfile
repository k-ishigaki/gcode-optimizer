ARG BASE_IMAGE
# hadolint ignore=DL3006
FROM ${BASE_IMAGE}

ARG USER_ID
ARG GROUP_ID

RUN if command -v apk > /dev/null 2>&1; then \
        apk add --no-cache shadow=~4 sudo=~1; \
    else \
        apt-get update && apt-get install -y --no-install-recommends \
        sudo='1.*' \
        && apt-get clean && rm -rf /var/lib/apt/lists/*; \
    fi

RUN if [ -z "$(awk -v gid=${GROUP_ID} -F: '$3 == gid {print $1}' /etc/group)" ]; then \
        groupadd --gid ${GROUP_ID} developer; \
    fi && \
    if [ -z "$(awk -v uid=${USER_ID} -F: '$3 == uid {print $1}' /etc/passwd)" ]; then \
        useradd --uid ${USER_ID} --gid ${GROUP_ID} -m developer; \
    fi && \
    user_name="$(awk -v uid=${USER_ID} -F: '$3 == uid {print $1}' /etc/passwd)" && \
    echo "${user_name} ALL=(ALL) NOPASSWD: ALL" >> "/etc/sudoers.d/${user_name}" && \
    chown -R ${USER_ID}:${GROUP_ID} /root

ENV HOME=/root