This package provides `hoon-mode`, which provides syntax highlighting
for the language Hoon which is part of the [Urbit](https://urbit.org)
ecosystem.


## Installing
Currently this package must be installed manually.


## Configuring
Add the following to your configuration:

	(add-hook 'hoon-mode
	          (lambda ()
	            (define-key hoon-mode-map (kbd "C-c r") 'hoon-eval-region-in-urb)
	            (define-key hoon-mode-map (kbd "C-c b") 'hoon-eval-buffer-in-urb)))
