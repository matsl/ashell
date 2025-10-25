# ashell
Async Shell With Buffer History

AShell adds buffer history to `async-shell-command` buffers. Each
invocation of `ashell-command` is run in its own unique buffer keeping
the result for later review.

To not overflow Emacs with possibly uninteresting buffers there is a
configurable cap on how many buffers are kept,
`ashell-output-buffer-history-size`. The youngest buffers are kept.
Buffers with active processes or buffers set to read only state are
also kept and are not subject for removal.

To view the history of commands or act on the buffers there is an
adoption of ibuffer, `ashell-ibuffer`.

## Key bindings

AShell mode sets the following keys.

```el
(define-key ashell-command-mode-map (kbd "C-c C-r") #'ashell-rerun)
(define-key ashell-command-mode-map (kbd "C-c C-p") #'ashell-buffer-prev)
(define-key ashell-command-mode-map (kbd "C-c C-n") #'ashell-buffer-next)
(define-key ashell-command-mode-map (kbd "C-c C-b") #'ashell-ibuffer)
```

The following key binding are suggestions for global bindings you
might want to use or you can choose something else.

```el
(define-key esc-map "&" 'ashell-command)
(global-set-key "\C-c\M-&" 'ashell-ibuffer)
```

## Hyperbole support

Load hib-ashell.el to replace Hyperboles action types `exec-shell-cmd`
and `exec-window-cmd`.
