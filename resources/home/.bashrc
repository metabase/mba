PATH=~/bin/:$PATH
export HISTSIZE=100000
#PROMPT_COMMAND='history -a'
shopt -s histappend
export HISTFILESIZE=100000
export HISTCONTROL=ignorespace:erasedups

# https://www.metabase.com/docs/latest/operations-guide/environment-variables.html

mute() {
  $@ >/dev/null 2>/dev/null
}

die() {
  echo "$@"
  exit 1
}

deps() {
  for dep in "$@"; do
    mute which "$dep" || apt install -y "$dep" || die "$dep dependency missing"
  done
}

mb-setup() {
  deps jo httpie jq
  http :3000/api/setup \
       token=$(http :3000/api/session/properties | jq -r '.["setup-token"]') \
       user:=$(jo email=awesome@example.com\
                  first_name=asdf\
                  last_name=asdf\
                  password=lazyfox1) \
       prefs:=$(jo allow_tracking=false \
                   site_name=mysite)
}

mute which less || alias less=more
