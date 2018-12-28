#!/bin/bash
#
# Sync all the dotfiles across the system.
#

if [ "$1" == "there" ]; then
	from="."
	to="$HOME";
	str="from here to there"

	read -p "$str Continue (Y/n)? " choice
else
	from="$HOME"
	to="."
	str="from there to here"
	choice="y"
fi

case "$choice" in
	y|Y )
	# Emacs is my main programming envirenment.
	echo "Syncing Emacs configuration.."
	cp -TRv	"$from/.emacs.d/lisp/" "$to/.emacs.d/lisp/"
	cp -TRv	"$from/.emacs.d/fonts/" "$to/.emacs.d/fonts/"
	cp -v "$from/.emacs.d/init.el" "$to/.emacs.d/init.el"
	cp -v "$from/.emacs.d/pkgs.el" "$to/.emacs.d/pkgs.el"
	cp -v "$from/.emacs.d/configs.el" "$to/.emacs.d/configs.el"
	cp -v "$from/.emacs.d/defuns.el" "$to/.emacs.d/defuns.el"
	cp -v "$from/.emacs.d/modes.el" "$to/.emacs.d/modes.el"
	cp -v "$from/.emacs.d/custom.el" "$to/.emacs.d/custom.el"
	cp -v "$from/.emacs.d/beta.el" "$to/.emacs.d/beta.el"
	cp -v "$from/.emacs.d/kbds.el" "$to/.emacs.d/kbds.el"
	echo ""

	# I not relaying on vim as my main programming environment
	# but some times it becomes really handy
	echo "Syncing Vim configuration.."
	cp -TRv "$from/.vim" "$to/.vim"
	cp -TRv "$from/.vimrc"	"$to/.vimrc"
	cp -TRv "$from/.bin" "$to/.bin"
	echo ""

	echo "Syncing Other configrations.."
	cp -v "$from/.bashrc" "$to/.bashrc"
	cp -TRv "$from/.zsh" "$to/.zsh"
	cp -v "$from/.zshrc" "$to/.zshrc"
	cp -v "$from/.gitconfig" "$to/.gitconfig"
	cp -v "$from/.xboardrc" "$to/.xboardrc"
	# those defaults might not be preset in all systems
	cp -v "$from/.Xdefaults" "$to/.Xdefaults" 2>/dev/null
	cp -TRv "$from/.templates" "$to/.templates"
	echo "";;
	n|N ) echo "good choice";;
	* ) echo "invalid";;
esac
