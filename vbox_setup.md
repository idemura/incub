## Install `sudo`
```
su
apt-get install -y sudo
usermod -aG sudo igor
reboot
```

## Basic Packages
```
vi ~/.bashrc # In PS1 replace \w to \W
sudo apt-get install -y build-essential git vim gdb linux-tools tmux atop python-dev libtool m4 autoconf pkg-config cmake
```

## Customize
```
git clone https://github.com/idemura/incub.git
cd ~/incub/configs
cp .* ~
reboot
```

## Watchman
```
cd
git clone https://github.com/facebook/watchman.git
cd watchman
git checkout v4.9.0
sudo apt-get install -y libssl-dev
./autogen.sh && ./configure
make -j4 && sudo make install
make clean
```

## Nuclide Server
First, install WatchDog
```
wget -q -O - https://deb.nodesource.com/setup_11.x | sudo -E bash -
sudo apt-get install -y nodejs
sudo npm install -g nuclide
```

## Install Google
```
cd
git clone https://github.com/gflags/gflags.git
cd gflags
cmake . -DBUILD_SHARED_LIBS=ON
make -j4 && sudo make install
sudo ldconfig
make clean

cd
git clone https://github.com/google/glog.git
cd glog
./autogen.sh && ./configure
make -j4 && sudo make install
sudo ldconfig
make clean

cd
git clone https://github.com/google/googletest.git
cd googletest
cmake .
make -j4 && sudo make install
make clean
```

## Install LLVM/Clang/LLDB
```
sudo mkdir /opt/LLVM
sudo chown igor:igor /opt/LLVM -R
cd /opt/LLVM

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

cd /opt/LLVM
mkdir build
cd build
sudo apt-get install -y libncurses5-dev libxml2-dev libpcre3 libpcre3-dev libedit-dev

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
rm -rf build
```
