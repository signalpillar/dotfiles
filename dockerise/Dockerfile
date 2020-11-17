FROM ubuntu

# https://askubuntu.com/questions/876240/how-to-automate-setting-up-of-keyboard-configuration-package
ENV DEBIAN_FRONTEND noninteractive

# apt-add-repository is just not in the base Ubuntu image
RUN apt-get -qq update \
  && apt-get install -y software-properties-common \
  && apt-add-repository ppa:ubuntu-elisp/ppa \
  && apt-get -qq update

RUN apt-get install -y git tmux emacs-snapshot emacs-snapshot
RUN apt-get install -y tcpdump
RUN apt-get install -y strace
RUN apt-get install -y curl wget golang \
    make build-essential libssl-dev zlib1g-dev libbz2-dev libreadline-dev \
    libsqlite3-dev llvm libncurses5-dev \
    libncursesw5-dev xz-utils tk-dev libffi-dev liblzma-dev python-openssl

# source: https://github.com/marvinpinto/kitchensink/blob/master/Dockerfile
RUN apt-get install -y \
    vim \
    diffstat \
    pkg-config \
    cmake \
    tcpdump \
    wget \
    host \
    dnsutils \
    tree \
    dos2unix \
    zip \
    bash-completion \
    aspell \
    aspell-en \
    libjpeg-dev \
    automake \
    editorconfig \
    imagemagick \
    bc \
    lcov \
    man \
    gnupg2 \
    gnupg-agent \
    pinentry-curses \
    psmisc \
    apt-transport-https \
    libxcursor1 \
    libnss3 \
    libgconf-2-4 \
    libasound2 \
    libatk1.0-0 \
    libgtk-3-0 \
    locales \
    sudo \
    xterm \
    ffmpeg \
    libtool \
    libssl-dev \
    pass \
    iputils-ping \
    jq \
    pv \
    socat \
    silversearcher-ag \
  && apt-get clean autoclean \
  && apt-get autoremove -y --purge \
  && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* \
  && rm -rf /var/lib/{apt,dpkg,cache,log}/

# Configure timezone and locale
RUN apt-get -qq update \
  && apt-get install -y language-pack-en-base tzdata \
  && echo "UTC" > /etc/timezone \
  && echo "LANG=en_US.UTF-8" > /etc/default/locale \
  && echo "LC_ALL=en_US.UTF-8" >> /etc/default/locale \
  && LANGUAGE=en_US.UTF-8 LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 locale-gen --purge en_US.UTF-8 \
  && LANGUAGE=en_US.UTF-8 LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 dpkg-reconfigure -f noninteractive locales \
  && apt-get clean autoclean \
  && apt-get autoremove -y --purge \
  && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* \
  && rm -rf /var/lib/{apt,dpkg,cache,log}/

# Install 1password cli
RUN mkdir -p /tmp/op \
  && cd /tmp/op \
  && wget --no-verbose https://cache.agilebits.com/dist/1P/op/pkg/v0.8.0/op_linux_amd64_v0.8.0.zip \
  && unzip op_linux_amd64_v0.8.0.zip \
  && rm op*.zip \
  && mv op /usr/local/bin \
  && cd /tmp \
  && rm -rf op

# Install docker compose within the container
RUN curl -L "https://github.com/docker/compose/releases/download/1.24.1/docker-compose-Linux-x86_64" -o /usr/local/bin/docker-compose \
  && chmod +x /usr/local/bin/docker-compose

# Install mitmproxy
RUN mkdir -p /tmp/mitmproxy \
  && cd /tmp/mitmproxy \
  && wget --no-verbose -O mitmproxy.tar.gz https://snapshots.mitmproxy.org/4.0.4/mitmproxy-4.0.4-linux.tar.gz \
  && tar xf mitmproxy.tar.gz \
  && rm -f mitmproxy.tar.gz \
  && mv mitm* /usr/local/bin/ \
  && cd /tmp \
  && rm -rf mitmproxy

# Install a recentish version of fzf
RUN mkdir -p /tmp/fzf \
  && cd /tmp/fzf \
  && wget --no-verbose https://github.com/junegunn/fzf-bin/releases/download/0.21.1/fzf-0.21.1-linux_amd64.tgz \
  && tar xzf fzf-0.21.1-linux_amd64.tgz \
  && mv fzf /usr/local/bin/fzf \
  && cd /tmp \
  && rm -rf fzf

# Setup home environment
run useradd dev
run mkdir /home/dev && chown -R dev: /home/dev
run mkdir -p /home/dev/go /home/dev/bin /home/dev/lib /home/dev/include
env PATH /home/dev/bin:$PATH
env PKG_CONFIG_PATH /home/dev/lib/pkgconfig
env LD_LIBRARY_PATH /home/dev/lib
env GOPATH /home/dev/go:$GOPATH


# sudo placeholder for the scripts that rely on it
RUN echo '$@' > /bin/sudo && chmod +x /bin/sudo

# Create a shared data volume
# We need to create an empty file, otherwise the volume will
# belong to root.
# This is probably a Docker bug.
run mkdir /var/shared/
run touch /var/shared/placeholder
run chown -R dev:dev /var/shared
volume /var/shared

RUN chown -R dev: /home/dev
user dev

RUN curl -sfL https://git.io/chezmoi | sh
RUN rm -rf /var/lib/apt/lists/*

RUN git clone https://github.com/pyenv/pyenv.git ~/.pyenv
RUN echo 'export PYENV_ROOT="$HOME/.pyenv"' >> ~/.bashrc \
  && echo 'export PATH="$PYENV_ROOT/bin:$PATH"' >> ~/.bashrc \
  && echo 'if command -v pyenv 1>/dev/null 2>&1; then\n eval "$(pyenv init -)"\nfi' >> ~/.bashrc

RUN git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d && cd ~/.emacs.d && git checkout develop

ENV PATH=/root/.pyenv/bin:$PATH

RUN chezmoi init https://github.com/signalpillar/dotfiles/ && bash -c "chezmoi apply"
RUN git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
RUN pyenv install 3.7.8

RUN touch ~/.env
