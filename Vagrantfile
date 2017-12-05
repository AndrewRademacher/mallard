# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure("2") do |config|
    config.vm.box = "ubuntu/xenial64"
  
    config.vm.provider "virtualbox" do |vb|
      # Display the VirtualBox GUI when booting the machine
      vb.gui = false
    
      vb.cpus = 4
      vb.memory = "8000"
    end

    config.vm.network "private_network", ip: "172.28.128.3"
  
    config.vm.provision "shell" do |s|
      s.path = "vagrant/provision.sh"
      s.privileged = false
    end
  end
