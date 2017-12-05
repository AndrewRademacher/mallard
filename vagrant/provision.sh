sudo apt-get update
sudo apt-get install -y software-properties-common
sudo add-apt-repository -y ppa:hvr/ghc
sudo apt-get update
sudo apt-get install ghc-8.2.2 cabal-install-2.0 libpq-dev happy -y
sudo ln -s /opt/cabal/2.0/bin/cabal /usr/local/bin/cabal
sudo ln -s /opt/ghc/8.2.2/bin/ghc /usr/local/bin/ghc

cd /vagrant
cabal sandbox init
cabal update
cabal install --only-dependencies
