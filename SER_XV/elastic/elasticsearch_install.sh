#!/bin/sh
set -e

curl -L -O https://download.elasticsearch.org/elasticsearch/release/org/elasticsearch/distribution/tar/elasticsearch/2.1.1/elasticsearch-2.1.1.tar.gz
tar -zxvf elasticsearch-2.1.1.tar.gz

# sudo mv ./elasticsearch-2.1.1 /usr/local
# cd /usr/local
# rm -rf elasticsearch
# sudo ln -s elasticsearch-2.1.1 elasticsearch

# sudo apt-get install libcurl4-openssl-dev # może być wymagane do zainstalowania pakietu elastic w R
