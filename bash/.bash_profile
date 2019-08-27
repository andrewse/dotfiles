# For setting history length see HISTSIZE and HISTFILESIZE in bash(1)
# HISTSIZE=1000
# HISTFILESIZE=2000

# Eternal bash history.
# ---------------------
# Undocumented feature which sets the size to "unlimited".
# http://stackoverflow.com/questions/9457233/unlimited-bash-history
export HISTFILESIZE=
export HISTSIZE=
export HISTTIMEFORMAT="[%F %T] "
# Change the file location because certain bash sessions truncate .bash_history file upon close.
# http://superuser.com/questions/575479/bash-history-truncated-to-500-lines-on-each-login
export HISTFILE=~/.bash_eternal_history

# Force prompt to write history after every command.
# http://superuser.com/questions/20900/bash-history-loss
PROMPT_COMMAND="history -a; $PROMPT_COMMAND"

# Case-insensitive globbing (used in pathname expansion)
shopt -s nocaseglob;

# Append to the Bash history file, rather than overwriting it
shopt -s histappend;

# Autocorrect typos in path names when using `cd`
shopt -s cdspell;

# Enable some Bash 4 features when possible:
# * `autocd`, e.g. `**/qux` will enter `./foo/bar/baz/qux`
# * Recursive globbing, e.g. `echo **/*.txt`
for option in autocd globstar; do
	shopt -s "$option" 2> /dev/null;
done;

# Add tab completion for many Bash commands
if which brew &> /dev/null && [ -f "$(brew --prefix)/share/bash-completion/bash_completion" ]; then
	source "$(brew --prefix)/share/bash-completion/bash_completion";
elif [ -f /etc/bash_completion ]; then
	source /etc/bash_completion;
fi;

# Enable tab completion for `g` by marking it as an alias for `git`
if type _git &> /dev/null && [ -f /usr/local/etc/bash_completion.d/git-completion.bash ]; then
	complete -o default -o nospace -F _git g;
fi;

# Add tab completion for SSH hostnames based on ~/.ssh/config, ignoring wildcards
[ -e "$HOME/.ssh/config" ] && complete -o "default" -o "nospace" -W "$(grep "^Host" ~/.ssh/config | grep -v "[?*]" | cut -d " " -f2- | tr ' ' '\n')" scp sftp ssh;

# Add tab completion for `defaults read|write NSGlobalDomain`
# You could just use `-g` instead, but I like being explicit
complete -W "NSGlobalDomain" defaults;

# Add `killall` tab completion for common apps
complete -o "nospace" -W "Contacts Calendar Dock Finder Mail Safari iTunes SystemUIServer Terminal Twitter" killall;

# Add to the keychain identites that I want to be loaded on startup
# https://stackoverflow.com/questions/3466626/add-private-key-permanently-with-ssh-add-on-ubuntu
# ssh-add -K ~/.ssh/your_private_key


##### ALIASES #######
alias profile='mvim $HOME/.profile'
alias dockerstart='/Applications/Docker/Docker\ Quickstart\ Terminal.app/Contents/Resources/Scripts/start.sh'
alias jmakefu='./jmake cleanall && ./jmake install && ./jmake unit-tests'
alias jmakeci='./jmake cleanall && ./jmake install'
alias jmakecivd='./jmake clean && ./jmake install && ./jmake unit-tests --verify-deps'
alias jmakecd='./jmake cleanall && ./jmake debug'
alias jmaked='./jmake debug'
alias cc='git grep ">>>>>>>"'
alias ll='ls -lt'


alias passgp='pass git push'
alias portsinuse='lsof -iTCP -sTCP:LISTEN -n -P'
alias gnucash='wine /Users/asemple/.wine/drive_c/GnuCashPortable/GnuCashPortable.exe'
alias pwsafe='wine /Users/asemple/.wine/drive_c/Program\ Files/Password\ Safe/pwsafe.exe'
# alias mvim='mvim --remote-tab-silent'
alias colourdiff='colordiff'

alias mount_nas_tv='mkdir /Volumes/NasTv && mount_afp -i afp://andrew@nas-ds413j.local/Tv /Volumes/NasTv/'
alias mvim='mvim --remote-tab-silent'
alias brewdo='brew update && brew upgrade && brew cask upgrade && brew cleanup'
#alias ideamerge='/Applications/IntelliJ\ IDEA.app/Contents/MacOS/idea diff'

alias sssh='ondemand-commands-ssh '

#http://tldp.org/LDP/Bash-Beginners-Guide/html/sect_07_01.html
#Alias commands for git
alias grh='git reset --hard'
alias whcs='git diff-tree --no-commit-id --name-only -r'
alias gs='git status'
alias gnp='git --no-pager'
alias gitnp='git --no-pager'
#Git checkout
alias goco='git checkout'
alias gcom='git commit'

alias glol='git log --pretty=format:"%C(yellow)%<(15)%h%Creset%C(magenta)%<(30)%p%Creset%x09%<(30)%an%x09%Cred%ar%x09%Creset%s"'
alias glolnp='git --no-pager log --pretty=format:"%C(yellow)%<(10)%h%Creset%C(magenta)%<(20)%p%Creset%x09%<(20)%an%x09%Cred%ar%x09%Creset%s"'
alias gcd='git cherry -v'
alias gitcleanbranch='git branch --merged | egrep -v "(^\*|master)" | xargs git branch -d'
alias gitselectbranch='git branch | termenu | xargs git co'
alias ssh-proxy='ssh -D 9999 -f -C -N '
