# Why Doom?
I just gave Doom a spin, thinking of migrating my vanilla Emacs config to Doom [https://github.com/jethrokuan/dots/blob/d9ab45f13bd0b840b9f3f4b1ce2f76b71d06e935/.doom.d/config.el](https://github.com/jethrokuan/dots/blob/d9ab45f13bd0b840b9f3f4b1ce2f76b71d06e935/.doom.d/config.el) (he [wrote a blogpost about it](https://blog.jethro.dev/posts/migrating_to_doom_emacs/) and the benefits, namely its speed and the fact that "things just work").

# Installation
Following [Doom's "Getting Started" guide](https://github.com/hlissner/doom-emacs/blob/develop/docs/getting_started.org) for Mac OS X using Homebrew:

``` sh
# required dependencies
brew install git ripgrep
# optional dependencies
brew install coreutils fd
# Installs clang
xcode-select --install

brew tap d12frosted/emacs-plus
brew install emacs-plus
ln -s /usr/local/opt/emacs-plus/Emacs.app /Applications/Emacs.app

git clone https://github.com/hlissner/doom-emacs ~/.emacs.d
# Get rid of default config and install this config.
rm ~/.doom.d/*
git clone git@github.com:christabella/doom-emacs-config.git ~/.doom.d
~/.emacs.d/bin/doom install

```

Need [mplayer](https://mplayerosx.ch/) for nyan-mode music.

## Evil be gone!
Comment out the `evil` Doom module in `init.el` (after entering the Vi insert mode), `M-x doom-reload`, and restart Emacs.

## Note-taking
### Zotero library sync with Dropbox bib file 
See the Zotero section of https://rgoswami.me/posts/org-note-workflow/#zotero, and use http://zotfile.com/ to rename files to %b better biblatex citation key.

# Most-used keybindings
`C-a`, `C-e` are all "smart" in Doom, behaving how you would expect w.r.t. indents, comments etc.

A lot (but not all) of the functionality I previously relied on Crux for, are also in the `default` module:
``` emacs-lisp

   "s-<return>" #'default/newline-below
   "S-s-<return>" #'default/newline-above

```

`
# Mac OS X personal customizations
- Keyboard > Delay until repeat
- Trackpad > Speed
- Keyboard > Shortcuts > Input Sources > Disable `C-<Space>` shortcut
- Use Karabiner > Complex modifications > Add rule (from Internet) > Emacs key bindings (option,control+keys)

# Python
``` sh
pyenv install 3.6.10
pyenv global 3.6.10
# If you ever want to go back to the system version of Python as the default, you can run this:
# pyenv global system
```

 For example, with `pyenv-virtualenv` from the project's root:

``` sh
pyenv install 3.7.7
pyenv virtualenv 3.7.7 message-ranking
pyenv local message-ranking
pip install -r requirements.txt
```

