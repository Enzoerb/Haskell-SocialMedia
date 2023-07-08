FROM haskell:9.2.7

RUN mkdir /app
WORKDIR /app/backend

COPY ./*.cabal ./*.json /app/backend/
RUN cabal update && cabal build --only-dependencies -j4

COPY . /app/backend
RUN cabal build

CMD ["cabal", "run"]

