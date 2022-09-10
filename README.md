# Landmarks

This package provides so called "landmarks" to emacs.

Landmarks are a fast navigation facility for emacs, based on emacs
"registers", but faster to use. The main idea is to attach (some kind
of) registers to **keyboard keys** (typically a numpad key or an "f"
key) instead of characters. Hitting just one key makes the jump, and a
simple keystroke sets the landmark.

## Installation

Load the file `landmarks.el` and put it in a directory known to emacs.
Then add the following line to your init file:

```elisp
(require 'landmark)
```

## Configuration: a quick try

Follow one of the 2 sections below.

### If you have a numeric pad on your keyboard

Put this in your init file (or simply evaluate this directly in emacs
for testing purposes):

```elisp
(landmark-assign-kp-n-config) ;; if you have a numpad
```

Be aware that it loads predefined **global** keybindings for numpad
keys.

#### Quick Test

Now open a file, go somewhere and hit `C-S-kp-1` (control + shift +
numpad 1), then move the point around and hit `kp-1` (numpad 1), you
should jump immediately to the place where you hit `C-S-kp-1`. Notice
how by default the stored position is highlighted. This is
configurable (see section Explanations and configurations).

Open a new file, hit `kp-1`, you should be right back to previous
buffer at the same stored position.

Go back to the second buffer, hit `C-kp-2`. This time you memorized
the buffer, but no particular location in it. Hitting `kp-1` still
works, and `kp-2` makes you go back to the second buffer at current
point.

Mnemo: the `C-` prefix means 'store buffer only' and `C-S-` means
'store the precise location in the buffer'. The longer is the prefix,
the more precise is the landmark.

### Or if you have "fn" keys your keyboard

Put this in you ini file (or simply evaluate this directly in emacs
for testing purposes):

```elisp
(landmark-assign-fn-config)  ;; if you have "fn" keys
```

Be aware that it load predefined **global** keybindings to F5 to F9
keys.

Follow the same steps as in previous section "quick test", replacing
`kp-1` with `f1` and `kp-2` with `f2`.


## Explanations and Configurations

Similarly to an emacs registers, a *landmark* is a location where you
might want to come back later. Unlike registers it is either

- a buffer (jumping back to a \"buffer landmark\" jumps to the buffer
  at its current point position)
- or a precise position in a buffer (\"position landmark\").

Like registers, each landmark is identified uniquely by a character
but this is anecdotical. More importantly it is also (by default)
attached to a **key of your keyboard**.

For example, say we take the f1 key for landmark ?1, then the
following invocation:

`(landmark-assign-three-standard-keys ?1 \'f1)`

sets *global keybindings* such that:

- hitting C-f1 sets landmark ?1 to current buffer (function
  `landmark-of-buffer`).

- hitting C-S-f1 sets landmark ?1 to current position (function
  `landmark-of-position`).

- hitting f1 itself jumps to the landmark ?1 (which makes it much
  faster than any keybindings for registers) (function
  `landmark-jump`).

One can chose any key but chosing a self inserting key would be
harmful since the self insertion would be lost (unless you change
modifiers, see below). Numpad keys are a good choice:

```elisp
(landmark-assign-three-standard-keys ?0 'kp-0)
(landmark-assign-three-standard-keys ?0 'kp-insert)
```

See `landmark-assign-kp-n-config` to assign all numpad keys at once.

One can also change the modifiers (C- and C-S- above) at will
using the function `landmark-assign-keys`. Typically:

```elisp
(landmark-assign-keys ?1 [(meta kp-1)] [(control kp-1)] [(shift kp-1)])
(landmark-assign-keys ?1 [(meta kp-end)] [(control kp-end)] [(shift kp-end)])
```
(Be careful with shift and numpad, as shift changes kp-1 into kp-end).

### Visual feedback

Position landmarks are by default highlighted (box style). This is
configurable using `landmark-face`, `landmark-show-landmark-position`
and `landmark-show-landmark-fringe`.


### Note about numpad keys

Be careful with shift and numpad: the shift modifier changes kp-1 into
kp-end for instance (and similarly for all kp-xxx keys). you should do
some test to make this work with `landmark-assign-keys` as explained
above. The predefined `landmark-assign-kp-n-config` applies the
keybindings to both variants of each key.

### predefined settings

By default requiring `landmark` does not create any keybinding. Two
functions allow to set predefined *global* keybindings:

```elisp
(landmark-assign-kp-n-config)
```
for binding `kp-0` to `kp-9`, and

```elisp
(landmark-assign-fn-config)
```
for binding `f-5` to `f-9`.

For more control see section Explanations and Configurations above.
