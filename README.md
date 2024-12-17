# cl-tetris3d

Yet another 3D tetris.

![Screenshot](https://paste.opensuse.org/images/53051428.jpg)

## How to run:

``` lisp
> (ql:quickload :cl-tetris3d)
> (cl-tetris3d:run)
```

Enjoy!

## Controls:

  * left/right - move figure left/right
  * down - land figure
  * up - rotate figure
  * space - pause/unpause
  * esc - quit

## Make executable (SBCL Only!):

There are two ways to do it:

1. Run interpeter from shell (it doesn't work within Slime),
and run the following commands from REPL:
```
> (ql:quickload :cl-tetris3d)
> (cl-tetris3d:make-executable)
```

2.  Run ```./sbcl-make-executable.run``` script

After that you should get the ```./cl-tetris3d``` binary.


