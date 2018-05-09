#!/bin/bash

cp grammar.cf build/
docker exec -ti lucid_wozniak "bnfc -m -haskell grammar.cf"
docker exec -ti lucid_wozniak make
cp build/AbsGrammar.hs src/
sudo chown -R mwarzynski build
cp Makefile build/
