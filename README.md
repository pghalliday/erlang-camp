erlang-camp
===========

Pre-requisites
--------------

- [VirtualBox 4.2.16](https://www.virtualbox.org/wiki/Downloads)
- [Vagrant 1.2.7](http://downloads.vagrantup.com/)
- The vagrant bindler plugin

```
$ vagrant plugin install bindler
$ vagrant bindler setup
```

Starting the vagrant development environment
--------------------------------------------

- Clone the project and switch to the project directory

```
~$ git clone https://github.com/pghalliday/erlang-camp.git
~$ cd erlang-camp
```

- Install vagrant dependencies, start the vagrant VM and SSH into it

```
~/erlang-camp$ vagrant plugin bundle
~/erlang-camp$ vagrant up
~/erlang-camp$ vagrant ssh
```

- Switch to the project directory in the VM

```
vagrant@erlang-camp:~$ cd /vagrant/
```