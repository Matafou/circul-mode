# Landmarks

## Installation and a quick try

Load the file landmarks and put it in a directory known to emacs. then
put this in your init file:

### If you have a numeric pad on your keyboard

```lisp
(require 'landmark)
(landmark-assign-kp-n-config) ;; if you have a numpad
```

Be aware that these lines assign global keybindings.

Now open a file, go somewhere and hit `C-S-kp-1` (control + numpad 0),
then move move the point around and hit `kp-1` (numpad 0), you should
jump immediately to the place where you hit `C-S-kp-1`.

Then open a new file, hit `kp-1`, you should be right back to previous buffer.

Go back to the second buffer, hit `C-kp-2`. This time you memorize the
buffer, but no particular location in it. `kp-1` and `kp-2` makes you
go back and forth.

### Or if you have "fn" keys your keyboard

```lisp
(require 'landmark)
(landmark-assign-fn-config)  ;; if you have "fn" keys
```


and follow the same steps as in previous section replacing `kp-1` with
`f1` and `kp-2` with `f2`.


## Explanations and configurations

Like emacs registers a landmark is a location where you might
want to come back later. Unlike registers it is

- either a buffer (jumping back to a \"buffer landmark\" jumps to the
  buffer at its current point position)
- or a precise position in a buffer (\"position landmark\").

Like registers, each landmark is identified uniquely by a character
but this is anecdotical. Unlike registers it is also attached to a key
of your keyboard.

Say we take the f1 key for landmark ?1, then the following:

`(landmark-assign-three-standard-keys ?1 \'f1)'

makes so that:

- hitting C-f1 sets landmark ?1 to current buffer `landmark-of-buffer'

- hitting C-S-f1 sets landmark ?1 to current position `landmark-of-position'.

- hitting f1 itself jumps to the landmark ?1 (which makes it much
  faster than any keybindings for registers) (`landmark-jump').

One can chose any key but chosing a self inserting key would be
harmful since the self insertion would be lost. Numpad keys
is a good choice:

  (landmark-assign-three-standard-keys ?0 'kp-0)
  (landmark-assign-three-standard-keys ?0 'kp-insert)

See `landmark-assign-kp-n-config' to assign all numpad keys at once.

One can also change the modifiers (C- and C-S- above) at will
using the function `landmark-assign-keys'. Typically:

  (landmark-assign-keys ?1 [(meta kp-1)] [(control kp-1)] [(shift kp-1)])
  (landmark-assign-keys ?1 [(meta kp-end)] [(control kp-end)] [(shift kp-end)])

(Be careful with shift and numpad, as shift changes kp-1 into kp-end).

Positional landmarks are (by default) visible in the buffer. This
is controlled by `landmark-face', `landmark-show-landmark-position' and
`landmark-show-landmark-fringe'.
