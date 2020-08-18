#!/bin/bash

rsync -az --progress --no-owner --no-group --no-perms --update \
  tiger:bird-niche-models/data/points $HOME/local/bird-data

rsync -az --progress --no-owner --no-group --no-perms --update \
  tiger:bird-niche-models/data/models $HOME/local/bird-data

rsync -az --progress --no-owner --no-group --no-perms --update \
  tiger:bird-niche-models/data/maps $HOME/local/bird-data
