FROM ubuntu
RUN apt-get update &&  apt-get install -y software-properties-common && \ 
  add-apt-repository -y ppa:hvr/ghc && \
  apt-get update && \
  apt-get install -y cabal-install-1.20 ghc-7.8.4
RUN echo 'export PATH=~/.cabal/bin:/opt/cabal/1.20/bin:/opt/ghc/7.8.4/bin:$PATH' > ~/.bashrc
RUN PATH=~/.cabal/bin:/opt/cabal/1.20/bin:/opt/ghc/7.8.4/bin:$PATH cabal update 
RUN apt-get install zlib1g-dev
RUN PATH=~/.cabal/bin:/opt/cabal/1.20/bin:/opt/ghc/7.8.4/bin:$PATH cabal install cabal-install
RUN PATH=~/.cabal/bin:/opt/cabal/1.20/bin:/opt/ghc/7.8.4/bin:$PATH cabal install alex happy