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
cd ~/.emacs.d
git reset --hard f7431aa7c98b27b337ccb30a4478d23f2aa7f263
# Get rid of default config and install this config.
rm -rf ~/.doom.d/
git clone git@github.com:christabella/doom-emacs-config.git ~/.doom.d
~/.emacs.d/bin/doom install

```

Alternatively, using https://github.com/railwaycat/homebrew-emacsmacport:
``` sh
brew tap railwaycat/emacsmacport
brew install emacs-mac
ln -s /usr/local/opt/emacs-mac/Emacs.app /Applications/Emacs.app
```

A few more things:
- `pip install python-language-server` for lsp-mode.
- Install fonts Iosevka and ETBembo.
- Set up Dropbox sync for `~/org` folder.
- Need [mplayer](https://mplayerosx.ch/) for nyan-mode music.

## Evil be gone!
Comment out the `evil` Doom module in `init.el` (after entering the Vi insert mode), `M-x doom-reload`, and restart Emacs.

## Note-taking
### Zotero library sync with Dropbox bib file
Install [Better Bibtex](https://retorque.re/zotero-better-bibtex/) and under Zotero Preferences (`Cmd-,`), set "Citation key format" to `[auth:lower][year]_[Title:skipwords:select=1,3:lower:condense=_]`

Next, use http://zotfile.com/ to rename files to %b better biblatex citation key.

See the Zotero section of https://rgoswami.me/posts/org-note-workflow/#zotero for more details.

# Most-used keybindings
`C-a`, `C-e` are all "smart" in Doom, behaving how you would expect w.r.t. indents, comments etc.

A lot (but not all) of the functionality I previously relied on Crux for, are also in the `default` module:
``` emacs-lisp

   "s-<return>" #'default/newline-below
   "S-s-<return>" #'default/newline-above

```

# Mac OS X personal customizations
1. Use Karabiner and copy [this file]() to `~/.config/karabiner/karabiner.json`.
2. Preferences > Keyboard > Modifier keys > Karabiner VirtualHIDKeyboard > caps as ctrl, fn as null. 
3. Preferences > Trackpad > Speed, Pointer > Enable drag with three fingers
4. Copy [this file](https://gist.github.com/christabella/e53ab79d02ca9e169cbc473da32b1470) as `~/Library/KeyBindings/DefaultKeyBinding.dict` to override OS X keybindings with Emacs ones. 
5. Install iTerm2 with [fairyfloss](https://github.com/aquartier/fairyfloss/blob/master/fairyfloss.itermcolors) profile > import colors.
6. In iTerm2 preferences, set option keys to Esc+ (to avoid typing integral symbols when doing `M-f/b`). 
7. Install oh-my-zsh with `agnoster` theme and set either Iosevka (has powerline glyphs out of the box) or a [powerline-patched](https://github.com/powerline/fonts) font in iTerm2 preferences. 

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

