local ret_status="%(?:%{$fg_bold[green]%}%%:%{$fg_bold[red]%}%%)%{$reset_color%}"
PROMPT='$(git_prompt_info)%{$fg[blue]%}%c%{$reset_color%} ${ret_status} '

ZSH_THEME_GIT_PROMPT_PREFIX="(%{$fg_bold[yellow]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}) "
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg_bold[yellow]%}*"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[blue]%}"