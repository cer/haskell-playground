FROM haskell
RUN apt-get install wget
RUN mkdir test
WORKDIR test
RUN wget http://www.stackage.org/lts/cabal.config
RUN . ~/.bashrc ; cabal update                       
RUN echo 'export LANG=C.UTF-8' >> ~/.bashrc
RUN . ~/.bashrc ; cabal install --force-reinstalls alex 
RUN . ~/.bashrc ; cabal install --force-reinstalls happy
RUN . ~/.bashrc ; cabal install --force-reinstalls yesod-bin 
RUN . ~/.bashrc ; cabal install base yesod-core
ADD hello.cabal .
RUN . ~/.bashrc ; cabal install -j --enable-tests --max-backjumps=-1 --reorder-goals
RUN mkdir /app
WORKDIR /app

