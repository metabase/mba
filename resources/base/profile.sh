make_prompt (){
  _prompt_res=$?

  # This prompt has only been tested with bash. Remove this if it works
  # anywhere else
  if [ -z "$BASH_VERSION" ]; then return; fi

  [ "$_prompt_res" = 0 ] && _prompt_hint="\[\033[1;34m\]" || _prompt_hint="\[\033[1;91m\]"
  [ "$PWD" = "$HOME" ] && _prompt_path="~" || _prompt_path=$PWD
  [ "$USER" = "root" ] && _prompt_usr="#" || _prompt_usr="$"

  PS1="\[\e[00m\]$_prompt_hint[$MBA_PREFIX:\[\033[1;92m\]$_prompt_path$_prompt_hint]$_prompt_usr\[\033[00m\] "
}

USER=`whoami`
PROMPT_COMMAND=make_prompt
