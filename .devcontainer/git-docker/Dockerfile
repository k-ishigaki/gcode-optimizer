FROM alpine
LABEL maintainer="Kazuki Ishigaki<k-ishigaki@frontier.hokudai.ac.jp>"

RUN apk add --no-cache bash git git-perl less shadow sudo

# install git-subrepo
SHELL [ "/bin/bash", "-c" ]
RUN git clone https://github.com/ingydotnet/git-subrepo && \
    echo 'source /git-subrepo/.rc' >> ~/.bashrc
ENV BASH_ENV ~/.bashrc

COPY .gitconfig /root/.gitconfig

ENV GIT_SUBREPO_ROOT /git-subrepo
ENV MANPATH /git-subrepo/man:
ENV PATH /git-subrepo/lib:$PATH

RUN find ${HOME} | xargs -n 50 -P 4 chmod o+rwx

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
ENV HOME=/root

CMD ["git"]
