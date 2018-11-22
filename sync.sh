#!/bin/sh
#
# Sync all the dotfiles across the system.
#
# TODO: add `import from` and `export to` option

# Emacs is my main programming envirenment.
echo "Syncing Emacs configuration..";
cp -TRv ~/.emacs.d/lisp/ .emacs.d/lisp/;
cp -v ~/.emacs.d/init.el .emacs.d/init.el;
cp -v ~/.emacs.d/defuns.el .emacs.d/defuns.el;
cp -v ~/.emacs.d/modes.el .emacs.d/modes.el;
cp -v ~/.emacs.d/custom.el .emacs.d/custom.el;
cp -v ~/.emacs.d/beta.el .emacs.d/beta.el;
cp -v ~/.emacs.d/keybindings.el .emacs.d/keybindings.el;
echo "";

# I not relaying on vim as my main programming environment
# but some times it becomes really handy
echo "Syncing Vim configuration..";
cp -TRv ~/.vim/ .vim/;
cp -TRv ~/.vimrc .vimrc;
cp -TRv ~/.bin/ .bin/;
echo "";

echo "Syncing Other configrations..";
cp -v ~/.bashrc .bashrc;
cp -v ~/.gitconfig .gitconfig;
cp -v ~/.xboardrc .xboardc;
cp -v ~/.Xdefaults .Xdefaults;
cp -TRv ~/.templates/ .templates
echo "";

return 0;
