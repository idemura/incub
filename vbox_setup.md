## Install `sudo`
```
su
apt-get install -y sudo
usermod -aG sudo igor
reboot
```

## Basic Packages
```
cd
vi .bashrc    # In $PS1 replace \w to \W
vi .profile   # export EDITOR=vi
sudo apt-get install -y build-essential git vim gdb linux-tools tmux atop python-dev libtool m4 autoconf pkg-config cmake dkms
```

## Customize
```
mkdir github && cd github
git clone https://github.com/idemura/incub.git
cd ~/github/incub/configs
cp -r incub/configs/. ~
reboot
```

## Watchman
```
cd ~/github
git clone https://github.com/facebook/watchman.git
cd watchman
git checkout v4.9.0
sudo apt-get install -y libssl-dev
mkdir cmake_build && cd cmake_build && cmake .. -DCMAKE_BUILD_TYPE=Release
make -j4 && sudo make install
cd .. && rm -rf cmake_build
```

## Nuclide Server
First, install Watchman
```
wget -q -O - https://deb.nodesource.com/setup_11.x | sudo -E bash -
sudo apt-get install -y nodejs
sudo npm install -g nuclide
```

## Install Google
```
cd ~/github
git clone https://github.com/gflags/gflags.git
cd gflags
mkdir cmake_build && cd cmake_build && cmake .. -DCMAKE_BUILD_TYPE=Release -DBUILD_SHARED_LIBS=ON
make -j4 && sudo make install
sudo ldconfig
cd .. && rm -rf cmake_build

cd ~/github
git clone https://github.com/google/glog.git
cd glog
mkdir cmake_build && cd cmake_build && cmake .. -DCMAKE_BUILD_TYPE=Release
make -j4 && sudo make install
sudo ldconfig
cd .. && rm -rf cmake_build

cd ~/github
git clone https://github.com/google/googletest.git
cd googletest
mkdir cmake_build && cd cmake_build && cmake .. -DCMAKE_BUILD_TYPE=Release
make -j4 && sudo make install
cd .. && rm -rf cmake_build
```

## Install LLVM/Clang/LLDB
```
sudo mkdir /opt/LLVM
sudo chown igor:igor /opt/LLVM -R
cd /opt/LLVM
sudo apt-get install -y libncurses5-dev libxml2-dev libpcre3 libpcre3-dev libedit-dev

LLVM_VER=7.0.1
SWIG_VER=3.0.12

wget -q http://releases.llvm.org/${LLVM_VER}/llvm-${LLVM_VER}.src.tar.xz
xz --decompress llvm-${LLVM_VER}.src.tar.xz
tar -xf llvm-${LLVM_VER}.src.tar
rm llvm-${LLVM_VER}.src.tar

cd llvm-${LLVM_VER}.src/tools
wget -q http://releases.llvm.org/${LLVM_VER}/cfe-${LLVM_VER}.src.tar.xz
xz --decompress cfe-${LLVM_VER}.src.tar.xz
tar -xf cfe-${LLVM_VER}.src.tar
rm cfe-${LLVM_VER}.src.tar
mv cfe-${LLVM_VER}.src clang

wget -q http://releases.llvm.org/${LLVM_VER}/lldb-${LLVM_VER}.src.tar.xz
xz --decompress lldb-${LLVM_VER}.src.tar.xz
tar -xf lldb-${LLVM_VER}.src.tar
rm lldb-${LLVM_VER}.src.tar
mv lldb-${LLVM_VER}.src lldb

wget -q http://releases.llvm.org/${LLVM_VER}/compiler-rt-${LLVM_VER}.src.tar.xz
xz --decompress compiler-rt-${LLVM_VER}.src.tar.xz
tar -xf compiler-rt-${LLVM_VER}.src.tar
rm compiler-rt-${LLVM_VER}.src.tar
mv compiler-rt-${LLVM_VER}.src compiler-rt

cd /opt/LLVM
mkdir cmake_build && cd cmake_build

# Custom SWIG:
wget -q http://prdownloads.sourceforge.net/swig/swig-${SWIG_VER}.tar.gz
tar -zxf swig-${SWIG_VER}.tar.gz
rm swig-${SWIG_VER}.tar.gz
cd swig-${SWIG_VER}
./autogen.sh && ./configure
make -j4 && sudo make install
make clean
cd ..

cmake -G "Unix Makefiles" ../llvm-${LLVM_VER}.src -DCMAKE_BUILD_TYPE=Release
make -j4 && sudo make install
cd ..
rm -rf cmake_build
```

## Shared Folders
Mount Guest Additions
```
sudo opt-get install -y dkms
sudo mkdir /mnt/vbox_additions
sudo mount /dev/cdrom /mnt/vbox_additions
cd /mnt/vbox_additions
sudo ./VBoxLinuxAdditions.run

# If didn't help, do:
cd /opt/VBoxGuestAdditions-*/init  
sudo ./vboxadd setup

sudo adduser $USER vboxsf
sudo mount -t vboxsf shared /home/$USER/shared
```
