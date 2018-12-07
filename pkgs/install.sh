# install packages for ubuntu
# TODO: automatically select system file

# ubuntu installation, and probably debian
sudo dpkg --set-selections < ./ubuntu.txt && sudo apt-get -u dselect-upgrade
