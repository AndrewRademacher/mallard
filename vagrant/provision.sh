sudo apt-get update
sudo apt-get upgrade -y
sudo apt-get install -y software-properties-common build-essential
sudo add-apt-repository -y ppa:hvr/ghc
sudo apt-get update
sudo apt-get install -y \
    ghc-8.0.2 \
    cabal-install-2.0 \
    libpq-dev \
    postgresql \
    happy \
    ruby \
    ruby-dev \
    rubygems \
    build-essential
sudo ln -s /opt/cabal/2.0/bin/cabal /usr/local/bin/cabal
sudo ln -s /opt/ghc/8.0.2/bin/ghc /usr/local/bin/ghc

# Project Dependencies
cd /vagrant
cabal update
cabal install --only-dependencies --enable-tests

# Database Setup
sudo su postgres -c "psql -c \"CREATE ROLE vagrant WITH LOGIN SUPERUSER PASSWORD 'password';\""
createdb vagrant

# Packaging Tools
# gem install -V --no-ri --no-rdoc fpm