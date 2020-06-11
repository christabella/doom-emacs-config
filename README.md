# Why Doom?
I just gave Doom a spin, thinking of migrating my vanilla Emacs config to Doom like Jethro did (he [wrote a blogpost about it](https://blog.jethro.dev/posts/migrating_to_doom_emacs/) and the benefits, namely its speed and the fact that "things just work").

## Installation
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
~/.emacs.d/bin/doom install
```

## Evil be gone!
Comment out the `evil` Doom module in `init.el` (after entering the Vi insert mode), `M-x doom-reload`, and restart Emacs. 



