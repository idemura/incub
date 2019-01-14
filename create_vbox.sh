#! /bin/bash

[[ $1 ]] || exit 1

NAME=$1
RAM=4096
CPU=4
DISK=30720
VDI="$HOME/VirtualBox VMs/$NAME/$NAME.vdi"
ISO="$HOME/Downloads/debian-9.6.0-amd64-netinst.iso"

vboxmanage createvm --name $NAME --ostype Debian_64 --register

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
        --natpf1 "ssh,tcp,,2022,,22" \
        --natpf1 "nuclide1,tcp,,9091,,9091" \
        --natpf1 "nuclide2,tcp,,9092,,9092" \
        --natpf1 "nuclide3,tcp,,9093,,9093" \
        --clipboard hosttoguest \
        --mouse ps2

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
