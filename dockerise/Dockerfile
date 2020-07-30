FROM ubuntu

# https://askubuntu.com/questions/876240/how-to-automate-setting-up-of-keyboard-configuration-package
ENV DEBIAN_FRONTEND noninteractive

# apt-add-repository is just not in the base Ubuntu image
RUN apt-get update \
  && apt-get install -y software-properties-common \
  && apt-add-repository ppa:ubuntu-elisp/ppa \
  && apt-get update
RUN apt-get install -y git tmux emacs-snapshot emacs-snapshot
RUN apt-get install -y curl wget golang \
    make build-essential libssl-dev zlib1g-dev libbz2-dev libreadline-dev \
    libsqlite3-dev llvm libncurses5-dev \
    libncursesw5-dev xz-utils tk-dev libffi-dev liblzma-dev python-openssl

RUN curl -sfL https://git.io/chezmoi | sh
RUN rm -rf /var/lib/apt/lists/*
# placeholder for the scripts that rely on it
RUN echo '$@' > /bin/sudo && chmod +x /bin/sudo

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
