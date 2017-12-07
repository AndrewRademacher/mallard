# sudo apt-get update
# sudo apt-get upgrade -y
# sudo apt-get install -y software-properties-common
# sudo add-apt-repository -y ppa:hvr/ghc
# sudo apt-get update
# sudo apt-get install \
#     ghc-8.2.2 \
#     cabal-install-2.0 \
#     libpq-dev \
#     postgresql \
#     happy -y
# sudo ln -s /opt/cabal/2.0/bin/cabal /usr/local/bin/cabal
# sudo ln -s /opt/ghc/8.2.2/bin/ghc /usr/local/bin/ghc

# cd /vagrant
# cabal update
# cabal install --only-dependencies

# sudo su postgres -c "psql -c \"CREATE ROLE vagrant WITH LOGIN SUPERUSER PASSWORD 'password';\""
# createdb vagrant

# New

sudo apt-get update
sudo apt-get upgrade -y
sudo apt install -y \
    libpq-dev \
    postgresql \
    curl

curl -sSL https://get.haskellstack.org/ | sh

cd /vagrant
stack setup
stack build

sudo su postgres -c "psql -c \"CREATE ROLE vagrant WITH LOGIN SUPERUSER PASSWORD 'password';\""
createdb vagrant