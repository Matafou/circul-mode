# Landmarks

## Installation and a quick try

Load the file landmarks and put it in a directory known to emacs. Then add the following line to your init file:

```elisp
(require 'landmark)
```

And follow one of the 2 sections below.

### If you have a numeric pad on your keyboard

Put this in your init file (or simply evaluate this directly in emacs
for testing purposes):

```elisp
(landmark-assign-kp-n-config) ;; if you have a numpad
```

Be aware that these lines assign global keybindings to you numpad keys.

Now open a file, go somewhere and hit `C-S-kp-1` (control + numpad 0),
then move the point around and hit `kp-1` (numpad 0), you should jump
immediately to the place where you hit `C-S-kp-1`.

Then open a new file, hit `kp-1`, you should be right back to previous
buffer at the same stored position.

Go back to the second buffer, hit `C-kp-2`. This time you memorize the
buffer, but no particular location in it. Hitting `kp-1` and `kp-2`
makes you go back and forth.

Memo: the `C-` prefix means 'store buffer only' and `C-S-` means
'store the precise location inthe buffer'.

### Or if you have "fn" keys your keyboard

```elisp
(require 'landmark)
(landmark-assign-fn-config)  ;; if you have "fn" keys
```

Be aware that these lines assign global keybindings to you F5 to F9
keys.


Follow the same tests as in previous section replacing `kp-1` with
`f1` and `kp-2` with `f2`.


## Explanations and configurations

Similarly to an emacs registers, a *landmark* is a location where you
might want to come back later. Unlike registers it is either

- a buffer (jumping back to a \"buffer landmark\" jumps to the buffer
  at its current point position)
- or a precise position in a buffer (\"position landmark\").

Position landmarks are (by default) visible in the buffer. This is
controlled by `landmark-face', `landmark-show-landmark-position' and
`landmark-show-landmark-fringe'.

Like registers, each landmark is identified uniquely by a character
but this is anecdotical. More importantly it is also (by default)
attached to a **key of your keyboard**.

For example, say we take the f1 key for landmark ?1, then the
following invocation:

`(landmark-assign-three-standard-keys ?1 \'f1)'

sets *global keybindings* such that:

- hitting C-f1 sets landmark ?1 to current buffer (function
  `landmark-of-buffer').

- hitting C-S-f1 sets landmark ?1 to current position (function
  `landmark-of-position').

- hitting f1 itself jumps to the landmark ?1 (which makes it much
  faster than any keybindings for registers) (function
  `landmark-jump').

One can chose any key but chosing a self inserting key would be
harmful since the self insertion would be lost (unless you change
modifiers, see below). Numpad keys are a good choice:

  (landmark-assign-three-standard-keys ?0 'kp-0)
  (landmark-assign-three-standard-keys ?0 'kp-insert)

See `landmark-assign-kp-n-config' to assign all numpad keys at once.

One can also change the modifiers (C- and C-S- above) at will
using the function `landmark-assign-keys'. Typically:

  (landmark-assign-keys ?1 [(meta kp-1)] [(control kp-1)] [(shift kp-1)])
  (landmark-assign-keys ?1 [(meta kp-end)] [(control kp-end)] [(shift kp-end)])

(Be careful with shift and numpad, as shift changes kp-1 into kp-end).


### Note about numpad keys

Be careful with shift and numpad: the shift modifier changes kp-1 into
kp-end for instance (and similarly for all kp-xxx keys). you should do
some test to make this work with `landmark-assign-keys` as explained
above. The predefined `landmark-assign-kp-n-config` applies the
keybindings to both variants of each key.
