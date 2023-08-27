This package provides `hoon-mode`, which provides syntax highlighting
for the language Hoon which is part of the [Urbit](https://urbit.org)
ecosystem. Additionally, it provides documentation extracted from
[developers.urbit.org] via `eldoc`.


## Installing
Currently this package must be installed manually.

For Doom Emacs, add the following to `/.doom.d/packages.el`:

``` emacs-lisp
(package! hoon-mode :recipe (:host github :repo "urbit/hoon-mode.el"))
```
For users of the [elpaca](https://github.com/progfolio/elpaca) package
manager and [use-package](https://github.com/jwiegley/use-package),
the installation may be done using

``` emacs-lisp
(use-package hoon-mode
  :elpaca (hoon-mode
           :host github
           :protocol ssh
           :repo "urbit/hoon-mode.el"
           :branch "master"
           :files (:defaults "*.json")))
```

## Configuring
Add the following to your configuration:

	(add-hook 'hoon-mode
	          (lambda ()
	            (define-key hoon-mode-map (kbd "C-c r") 'hoon-eval-region-in-herb)
	            (define-key hoon-mode-map (kbd "C-c b") 'hoon-eval-buffer-in-herb)))

## Documentation support
To conveniently read the documentation associated with a symbol, put
this in your configuration:

``` emacs-lisp
(setq eldoc-echo-area-prefer-doc-buffer t)
```
and open a documentation buffer with 'M-x `eldoc-doc-buffer`.

### Language Server
Install the [Hoon Language
Server](https://github.com/urbit/hoon-language-server)
and add the following to your configuration
``` emacs-lisp
(add-hook 'hoon-mode #'lsp)
```
