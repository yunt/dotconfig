# Path to your oh-my-zsh configuration.
ZSH=$HOME/.zsh

# Set to the name theme to load.
# Look in ~/.oh-my-zsh/temes/
ZSH_THEME="agnoster"
plugins=(git)
source $ZSH/oh-my-zsh.sh

#命令提示符 {{{
#case "$TERM" in
        #linux)
                #RPROMPT=$(echo '%{\033[31m%}%D %T%{\033[m%}')
                #PROMPT=$(echo '%{\033[34m%}%M%{\033[32m%}%/
                        #%{\033[36m%}%n%{\033[01;33m%} >>> %{\033[m%}')
                #;;
        #screen*|rxvt*|xterm*|sun)
                #. ~/.zshprompt
                #setprompt
                #;;
#esac
#}}}

#关于历史纪录的配置 {{{
#历史纪录条目数量
export HISTSIZE=10000
#注销后保存的历史纪录条目数量
export SAVEHIST=10000
#历史纪录文件
#export HISTFILE=~/.zhistory
#以附加的方式写入历史纪录
setopt INC_APPEND_HISTORY
#如果连续输入的命令相同，历史纪录中只保留一个
setopt HIST_IGNORE_DUPS      
#为历史纪录中的命令添加时间戳      
setopt EXTENDED_HISTORY      

#启用 cd 命令的历史纪录，cd -[TAB]进入历史路径
setopt AUTO_PUSHD
#相同的历史路径只保留一个
setopt PUSHD_IGNORE_DUPS

#在命令前添加空格，不将此命令添加到纪录文件中
#setopt HIST_IGNORE_SPACE      
#}}}

#{{{ ---[ Environment ]---------------------------------------------------
export PSPDEV="/usr/local/pspdev"
export PSPSDK="$PSPDEV/psp/sdk"
export PATH="$PATH:$PSPDEV/bin:$PSPSDK/bin"
export EDITOR=vi
export FLASH_ALSA_DEVICE=plug:dmix
## Unicode Locale
#export LANG=de_DE.UTF-8
#export LC_ALL=de_DE.UTF-8
export PS_PERSONALITY='linux'
#
## Manpath & Manualpage search order
export MANSECT=3:2:9:8:1:5:4:7:6:n
#
## Syntax highlight for less with 'source-highlite'
PAGER='less -X -M'
#export LESSOPEN="| /usr/local/bin/src-hilite-lesspipe.sh %s"
export LESS=' -R '
#}}}

#每个目录使用独立的历史纪录{{{
cd() {
    builtin cd "$@"                             # do actual cd
    fc -W                                       # write current history  file
    local HISTDIR="$HOME/.zsh_history$PWD"      # use nested folders for history
        if  [ ! -d "$HISTDIR" ] ; then          # create folder if needed
            mkdir -p "$HISTDIR"
        fi
        export HISTFILE="$HISTDIR/zhistory"     # set new history file
    touch $HISTFILE
    local ohistsize=$HISTSIZE
        HISTSIZE=0                              # Discard previous dir's history
        HISTSIZE=$ohistsize                     # Prepare for new dir's history
    fc -R                                       #read from current histfile
}
mkdir -p $HOME/.zsh_history$PWD
export HISTFILE="$HOME/.zsh_history$PWD/zhistory"

function allhistory { cat $(find $HOME/.zsh_history -name zhistory) }
function convhistory {
            sort $1 | uniq |
            sed 's/^:\([ 0-9]*\):[0-9]*;\(.*\)/\1::::::\2/' |
            awk -F"::::::" '{ $1=strftime("%Y-%m-%d %T",$1) "|"; print }'  
}
#使用 histall 命令查看全部历史纪录
function histall { convhistory =(allhistory) |
            sed '/^.\{20\} *cd/i\\' }
#使用 hist 查看当前目录历史纪录
function hist { convhistory $HISTFILE }

#全部历史纪录 top44
function top44 { allhistory | awk -F':[ 0-9]*:[0-9]*;' '{ $1="" ; print }' | sed 's/ /\n/g' | sed '/^$/d' | sort | uniq -c | sort -nr | head -n 44 }

#}}}

#杂项 {{{
#允许在交互模式中使用注释  例如：
#cmd #这是注释
setopt INTERACTIVE_COMMENTS
      
#启用自动 cd，输入目录名回车进入目录
#稍微有点混乱，不如 cd 补全实用
#setopt AUTO_CD
      
      
#禁用 core dumps
limit coredumpsize 0

#Emacs风格 键绑定
bindkey -e
#设置 [DEL]键 为向后删除
bindkey "\e[3~" delete-char

#以下字符视为单词的一部分
WORDCHARS='*?_-[]~=&;!#$%^(){}<>'
#}}}

#自动补全功能 {{{
setopt AUTO_LIST
setopt AUTO_MENU
#开启此选项，补全时会直接选中菜单项
#setopt MENU_COMPLETE

autoload -U compinit
compinit

#自动补全缓存
#zstyle ':completion::complete:*' use-cache on
#zstyle ':completion::complete:*' cache-path .zcache
#zstyle ':completion:*:cd:*' ignore-parents parent pwd

#自动补全选项
zstyle ':completion:*:match:*' original only
zstyle ':completion::prefix-1:*' completer _complete
zstyle ':completion:predict:*' completer _complete
zstyle ':completion:incremental:*' completer _complete _correct
zstyle ':completion:*' completer _complete _prefix _correct _prefix _match _approximate

#路径补全
zstyle ':completion:*' expand 'yes'
zstyle ':completion:*' squeeze-shlashes 'yes'
zstyle ':completion::complete:*' '\\'

zstyle ':completion:*' menu select
zstyle ':completion:*:*:default' force-list always

#彩色补全菜单 
eval $(dircolors -b) 
export ZLSCOLORS="${LS_COLORS}"
zmodload zsh/complist
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'

#错误校正      
zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*:match:*' original only
zstyle ':completion:*:approximate:*' max-errors 1 numeric

#kill 命令补全      
compdef pkill=kill
compdef pkill=killall
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:*:*:*:processes' force-list always
zstyle ':completion:*:processes' command 'ps -au$USER'

#补全类型提示分组 
zstyle ':completion:*:matches' group 'yes'
zstyle ':completion:*' group-name ''
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:options' auto-description '%d'
zstyle ':completion:*:descriptions' format $'\e[01;33m -- %d --\e[0m'
zstyle ':completion:*:messages' format $'\e[01;35m -- %d --\e[0m'
zstyle ':completion:*:warnings' format $'\e[01;31m -- No Matches Found --\e[0m'
#}}}

##行编辑高亮模式 {{{
# Ctrl+@ 设置标记，标记和光标点之间为 region
zle_highlight=(region:bg=magenta #选中区域 
               special:bold      #特殊字符
               isearch:underline)#搜索时使用的关键字
#}}}

##空行(光标在行首)补全 cd {{{
user-complete(){
    if [[ -n $BUFFER ]] ; then
        zle expand-or-complete
    else
        BUFFER="cd "
        zle end-of-line
        zle expand-or-complete
    fi }
zle -N user-complete
bindkey "\t" user-complete
#}}}

##在命令前插入 sudo {{{
#定义功能 
sudo-command-line() {
    [[ -z $BUFFER ]] && zle up-history
    [[ $BUFFER != sudo\ * ]] && BUFFER="sudo $BUFFER"
#光标移动到行末
    zle end-of-line
}
zle -N sudo-command-line
#定义快捷键为： [Esc] [Esc]
bindkey "\e\e" sudo-command-line
#}}}
  
#命令别名 {{{
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -i'
alias ls='ls -F --color=auto'
alias ll='ls -l'
alias la='ls -a'
alias rscp="rsync -ahP"
alias rsmv="rsync -ahP --remove-source-files"
alias grep='grep --color=auto'
alias ee='emacsclient -t'
#alias wine='env LANG=zh_CN.UTF-8 wine'
alias ntp='sudo ntpdate pool.ntp.org && sudo hwclock --systohc'
alias gae='/opt/google-appengine/appcfg.py'
alias gaetest='/opt/google-appengine/dev_appserver.py'
alias winxp-snap='VBoxManage snapshot winxp restore xp-snap'
#alias winxp='sudo modprobe vboxnetflt && VBoxSDL --startvm winxp --nofstoggle --noresize --evdevkeymap && sudo rmmod vboxnetflt vboxdrv'
alias winxp='VBoxSDL --startvm winxp --nofstoggle --noresize --evdevkeymap'
alias vless='vim -u /usr/share/vim/vim72/macros/less.vim'
alias emacs='env LC_CTYPE=zh_CN.UTF-8 emacs'

#[Esc][h] man 当前命令时，显示简短说明 
alias run-help >&/dev/null && unalias run-help
autoload run-help

#历史命令 top10
alias top10='print -l  ${(o)history%% *} | uniq -c | sort -nr | head -n 10'
#}}}

#路径别名 {{{
#进入相应的路径时只要 cd ~xxx
hash -d WWW="/home/lighttpd/html"
hash -d ARCH="/mnt/arch"
hash -d PKG="/var/cache/pacman/pkg"
hash -d E="/etc/env.d"
hash -d C="/etc/conf.d"
hash -d I="/etc/rc.d"
hash -d X="/etc/X11"
hash -d BK="/home/r00t/config_bak"
#}}}
    
##for Emacs {{{
#在 Emacs终端 中使用 Zsh 的一些设置 不推荐在 Emacs 中使用它
if [[ "$TERM" == "dumb" ]]; then
setopt No_zle
PROMPT='%n@%M %/
>>'
alias ls='ls -F'
fi 	
#}}}

####{{{
function timeconv { date -d @$1 +"%Y-%m-%d %T" }

zstyle ':completion:*:ping:*' hosts 129.27.2.3 www.tugraz.at \
       10.16.17.1{{7..9},}
# }}}

# ---[ Terminal settings ] {{{
case "$TERM" in
        linux)
                bindkey '\e[1~' beginning-of-line       # Home
                bindkey '\e[4~' end-of-line             # End
                bindkey '\e[3~' delete-char             # Del
                bindkey '\e[2~' overwrite-mode          # Insert
                ;;
        screen)
                # In Linux console
                bindkey '\e[1~' beginning-of-line       # Home
                bindkey '\e[4~' end-of-line             # End
                bindkey '\e[3~' delete-char             # Del
                bindkey '\e[2~' overwrite-mode          # Insert
                bindkey '\e[7~' beginning-of-line       # home
                bindkey '\e[8~' end-of-line             # end
                # In rxvt
                bindkey '\eOc' forward-word             # ctrl cursor right
                bindkey '\eOd' backward-word            # ctrl cursor left
                #bindkey '\e[3~' backward-delete-char    # This should not be necessary!
                #function sctitle() { print -Pn "\ek$1\e\\"}
                #function precmd() { sctitle "%20< ..<%~%<<" }
                #function preexec() { sctitle "%20>..>$1%< <" }

                ;;
        rxvt*)
                bindkey '\e[7~' beginning-of-line       # home
                bindkey '\e[8~' end-of-line             # end
                bindkey '\eOc'  forward-word             # ctrl cursor right
                bindkey '\eOd'  backward-word            # ctrl cursor left
                bindkey '\e[3~' delete-char    # This should not be necessary!
                bindkey '\e[2~' overwrite-mode          # Insert
                ;;
        xterm*)
                bindkey "\e[1~" beginning-of-line       # Home
                bindkey "\e[4~" end-of-line             # End
                bindkey '\e[3~' delete-char             # Del
                bindkey '\e[2~' overwrite-mode          # Insert
                ;;
        sun)
                bindkey '\e[214z' beginning-of-line       # Home
                bindkey '\e[220z' end-of-line             # End
                bindkey '^J'      delete-char             # Del
                bindkey '^H'      backward-delete-char    # Backspace
                bindkey '\e[247z' overwrite-mode          # Insert
                ;;
esac
#}}}

# ---Fix Title {{{
#screen integration to set caption bar dynamically
function title {
if [[ $TERM == "screen" || $TERM == "screen-256color-bce" || $TERM == "screen-bce" || $TERM == "screen.linux" ]]; then
    # Use these two for GNU Screen:
    print -nR $'\033k'$1$'\033'\\\

    print -nR $'\033]0;'$2$'\a'
elif [[ $TERM == "xterm" || $TERM == "urxvt" ]]; then
    # Use this one instead for XTerms:
    print -nR $'\033]0;'$*$'\a'
    #trap 'echo -ne "\e]0;$USER@$HOSTNAME: $BASH_COMMAND\007"' DEBUG
fi
}

#set screen title if not connected remotely
function precmd {
    title "`print -Pn "%~" | sed "s:\([~/][^/]*\)/.*/:\1...:"`" "$TERM $PWD"
    echo -ne '\033[?17;0;127c'
}

function preexec {
    emulate -L zsh
    local -a cmd; cmd=(${(z)1})
    if [[ $cmd[1]:t == "ssh" ]]; then
        title "@"$cmd[2] "$TERM $cmd"
    elif [[ $cmd[1]:t == "sudo" ]]; then
        title "#"$cmd[2]:t "$TERM $cmd[3,-1]"
    elif [[ $cmd[1]:t == "for" ]]; then
        title "()"$cmd[7] "$TERM $cmd"
    elif [[ $cmd[1]:t == "svn" ]]; then
        title "$cmd[1,2]" "$TERM $cmd"
    else
        title $cmd[1]:t "$TERM $cmd[2,-1]"
    fi
}
#}}}

# Less Colors for Man Pages
export LESS_TERMCAP_mb=$'\E[01;31m'       # begin blinking
export LESS_TERMCAP_md=$'\E[01;38;5;74m'  # begin bold
export LESS_TERMCAP_me=$'\E[0m'           # end mode
export LESS_TERMCAP_se=$'\E[0m'           # end standout-mode
export LESS_TERMCAP_so=$'\E[38;5;246m'    # begin standout-mode - info box
export LESS_TERMCAP_ue=$'\E[0m'           # end underline
export LESS_TERMCAP_us=$'\E[04;38;5;146m' # begin underline
 
# For wine
function prefix() {
      if [ -z "$1" ]; then 
            WINEPREFIX="$HOME/.wine/"
      else
            WINEPREFIX="$HOME/.local/share/wineprefixes/$1"
      fi
      export WINEPREFIX
}

function goc() {
      cd "$WINEPREFIX/drive_c"
}

function lsp() {
      ls $* "$HOME/.local/share/wineprefixes"
}

function run() {
      prefix "$1"
      goc
      wine cmd /c "run-$1.bat"
}

#complete -W "$(lsp)" prefix run

## END OF FILE #################################################################
# vim:filetype=zsh foldmethod=marker autoindent expandtab shiftwidth=4
