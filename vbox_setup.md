## Install `sudo`
```
su
apt-get install -y sudo
usermod -aG sudo igor
reboot
```

## Install basic packages
```
vi ~/.bashrc # In PS1 replace \w to \W
sudo apt-get install -y git vim gdb linux-tools tmux atop python-dev libtool m4 autoconf pkg-config cmake
mkdir github
cd github
git clone https://github.com/idemura/incub.git
```

## Customize configs
```
cd ~/github
cd incub/config
cp .* ~
reboot
```

## Watchdog
```
cd ~/github
git clone https://github.com/facebook/watchman.git
cd watchdog
git checkout v4.9.0
sudo apt-get install -y libssl-dev
./autogen.sh && ./configure
make -j4 && sudo make install
make clean
```

## Nuclide Server
First, install WatchDog
```
sudo apt-get install -y curl
curl -sL https://deb.nodesource.com/setup_10.x | sudo -E bash -
sudo apt-get install -y nodejs
sudo npm install -g nuclide
```

## Install Google
```
cd ~/github
https://github.com/gflags/gflags.git
cd gflags
cmake .
make -j4 && sudo make install
make clean

cd ~/github
git clone https://github.com/google/glog.git
cd glog
./autogen.sh && ./configure
make -j4 && sudo make install
make clean

cd ~/github
git clone https://github.com/google/googletest.git
cd googletest
cmake .
make -j4 && sudo make install
make clean

sudo ldconfig
```

## Install LLVM/Clang/LLDB
```
sudo mkdir /opt/LLVM
sudo chown igor:igor /opt/LLVM -R
cd /opt/LLVM
```
