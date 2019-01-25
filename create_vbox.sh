#! /bin/bash

[[ $1 ]] || exit 1

NAME=$1
RAM=4096
CPU=4
DISK=30720
VDI="$HOME/VirtualBox VMs/$NAME/$NAME.vdi"
ISO="$HOME/Downloads/debian-9.6.0-amd64-netinst.iso"

vboxmanage createvm --name $NAME --ostype Debian_64 --register

# vboxmanage hostonlyif create if vboxnet0 doesn't exist. Otherwise, check
# configuration: vboxmanage list hostonlyifs, see IP address and mask.
# To enable SSH, run on guest:
#   ip address
# Find enp0s8 without IP address and add into /etc/network/interfaces:
#   auto enp0s8
#   iface enp0s8 inet static
#   address 192.168.56.<number != 1>
#   netmask 255.255.255.0
vboxmanage modifyvm $NAME \
        --memory $RAM \
        --cpus $CPU \
        --vram 8 \
        --graphicscontroller vboxsvga \
        --boot1 disk \
        --boot2 dvd \
        --boot3 none \
        --boot4 none \
        --usbohci on \
        --usbehci on \
        --audio none \
        --pae off \
        --rtcuseutc on \
        --clipboard hosttoguest \
        --mouse ps2 \
        --nic2 hostonly \
        --hostonlyadapter2 vboxnet0

vboxmanage storagectl $NAME \
        --name SATA \
        --add sata \
        --controller IntelAHCI \
        --portcount 1 \
        --bootable on

vboxmanage storagectl $NAME \
        --name IDE \
        --add ide \
        --controller PIIX4 \
        --portcount 2 \
        --bootable on

vboxmanage createmedium disk --size $DISK --filename "$VDI"

vboxmanage storageattach $NAME \
        --storagectl SATA \
        --medium "$VDI" \
        --type hdd \
        --port 0 \
        --nonrotational on

vboxmanage storageattach $NAME \
        --storagectl IDE \
        --medium "$ISO" \
        --type dvddrive \
        --port 0 \
        --device 0

vboxmanage sharedfolder add $NAME \
        --name shared \
        --hostpath /Users/$USER/shared \
        --automount

