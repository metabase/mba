PATH=~/bin/:$PATH
export HISTSIZE=100000
#PROMPT_COMMAND='history -a'
shopt -s histappend
export HISTFILESIZE=100000
export HISTCONTROL=ignorespace:erasedups

mute() {
  $@ >/dev/null 2>/dev/null
}

mute which less || alias less=more
