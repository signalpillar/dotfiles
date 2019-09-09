ARG BASE_IMAGE=golang
ARG BASE_TAG=1.12.0-alpine3.9
FROM ${BASE_IMAGE}:${BASE_TAG} as base
# due diligence
RUN apk update --no-cache && apk upgrade --no-cache
# add common development tools
RUN apk add git make bash curl jq g++ ca-certificates \
   emacs \
   zsh \
   tmux \
   ncurses \
   curl \
   less

# add ssl capabilities
RUN update-ca-certificates

RUN adduser -D -s /bin/zsh -h /home/me me
WORKDIR /home/me
ENV HOME /home/me

RUN mkdir proj \
    && git clone https://github.com/signalpillar/dotfiles.git proj/dotfiles \
    && ln -s proj/dotfiles/.zshrc .zshrc \
    && ln -s proj/dotfiles/.zshenv .zshenv \
    && ln -s proj/dotfiles/.gitconfig .gitconfig \
    && ln -s proj/dotfiles/.spacemacs .spacemacs \
    && ln -s proj/dotfiles/functions.sh functions.sh

RUN curl -L https://github.com/robbyrussell/oh-my-zsh/raw/master/tools/install.sh | zsh || true

RUN touch .aliases .env

RUN git clone https://github.com/syl20bnr/spacemacs .emacs.d

CMD ["/bin/zsh"]
