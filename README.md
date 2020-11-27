This package provides `hoon-mode`, which provides syntax highlighting
for the language Hoon which is part of the [Urbit](https://urbit.org)
ecosystem.


## Installing
Currently this package must be installed manually.

For Doom Emacs, add the following to `/.doom.d/packages.el`:

``` emacs-lisp
(package! hoon-mode :recipe (:host github :repo "urbit/hoon-mode.el"))
```


## Configuring
Add the following to your configuration:

	(add-hook 'hoon-mode
	          (lambda ()
	            (define-key hoon-mode-map (kbd "C-c r") 'hoon-eval-region-in-herb)
	            (define-key hoon-mode-map (kbd "C-c b") 'hoon-eval-buffer-in-herb)))

### Language Server
Install the [Hoon Language
Server](https://github.com/urbit/hoon-language-server)
and add the following to your configuration
``` emacs-lisp
(add-hook 'hoon-mode #'lsp)
```

