export PATH=$HOME/bin:$PATH

# Color version:
PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\W\[\033[00m\]\$ '
# Plain:
# PS1='${debian_chroot:+($debian_chroot)}\u@\h:\W\$ '

alias ff='find -name'
alias ll='ls -l'
alias l1='ls -1'
alias la='la -1 -A'
alias b='./build'

if [ -d $HOME/profile.d ]; then
  for i in $HOME/profile.d/*.sh; do
    if [ -r $i ]; then
      . $i
    fi
  done
  unset i
fi

