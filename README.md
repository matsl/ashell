# ashell
Async Shell With Buffer History

AShell adds buffer history to `async-shell-command` buffers. Each
invocation of `ashell-command` is run in its own unique buffer keeping
the result for later review.

To not overflow Emacs with possibly uninteresting buffers there is a
configurable cap on how many buffers are kept,
`ashell-output-buffer-history-size`. The youngest buffers are kept.
Buffers with active processes or buffers set to read only state are
not subject for removal.

To view the history of commands or act on the buffers there is an
adoption of ibuffer, `ashell-ibuffer`.

## Example key bindings

```el
(define-key esc-map "&" 'ashell-command)
(global-set-key "\C-c&" 'ashell-ibuffer)
(define-key shell-command-mode-map "\C-cr" 'ashell-rerun)
```

## Hyperbole support

Load hib-ashell.el to replace Hyperboles action types `exec-shell-cmd`
and `exec-window-cmd`.

