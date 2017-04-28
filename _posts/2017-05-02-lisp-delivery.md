---
layout: post
title: Delivering games written in Common Lisp
permalink: delivering-common-lisp
excerpt_separator: <!--excerpt-->
---

Speaking generally, we can split end-users of a game into two groups: conventional gamers and
those who would like to tinker with game sources, if there's such option. Several ways exist to
serve needs of both groups. In this piece of writing we would look into two of them:
via [Quicklisp](https://www.quicklisp.org/) or bundling executable with required resources
(game assets, dynamic libraries, configs, etc) into downloadable package.

* [Quicklisp](delivering-common-lisp#quicklisp)
* [Bundle](delivering-common-lisp#bundle)

<!--excerpt-->

## Quicklisp

Users that would like to take a deep look into how a game actually works will appreciate this
method of delivery. Sharing CL code through Quicklisp is awesome, but couple of problems arise:
what if our code depends on a foreign dynamic library and how do we get access to game assets.


### Accessing assets

This problem is actually fairly easy to solve. Obviously, hardcoded absolute paths cannot be
used, but we can find a path to a system using `asdf` facilities if target system's definition
is already read, which is the case during system components loading:

``` common_lisp
;; if system's name is :awesome-game
;; and assets stored in the assets/ subdirectory of it
;; then we can find absolute path to the latter with
(defvar *assets-path* (asdf:system-relative-pathname :awesome-game #p"assets/"))

;; now we can access assets by merging *assets-path* pathname
;; with actual resource name
(with-open-file (sound-file (merge-pathnames "beep.ogg" *assets-path*))
  (awesome-game:import-asset sound-file))
```

### Providing dynamic libraries

Most wrappers or bindings require their users to install dynamic libraries manually, but that is
to much to ask if an application depends on the several of them.

Solution to that is to treat those libraries as assets, combine them into loadable asdf system
and open during load phase. Problem though, most wrappers try to open[^1] dynamic libraries very
early during their loading through `cffi`. Hopefully, `cffi` tries to use system's linker to
open the library first before searching for it in system directories, and if the library was
already opened a linker will recognize that and would not try to reload it[^2].

We can use this behaviour to our advantage by opening libraries provided by our asdf system
before wrappers will try to do the same. This way we can get libraries supplied with our asdf
system opened instead of default ones if those are present in the operating system's default
search directories.

``` common_lisp
;;; put dynamic libraries into lib/ subdirectory
;;; of your :awesome-game-libraries system

;;; in a file component of :awesome-game-libraries system

;; tell cffi where to find foreign libraries
(pushnew (asdf:system-relative-pathname :awesome-game-libraries #p"lib/")
         cffi:*foreign-library-directories*
         :test #'equal)

;; register libraries stored in lib/
(cffi:define-foreign-library libawesome
  (:darwin "libawesome.dylib")
  (:unix "libawesome.so")
  (:windows "libawesome.dll"))

(cffi:define-foreign-library librad
  (:darwin "librad.dylib")
  (:unix "librad.so")
  (:windows "librad.dll"))

;; and open/load them
(cffi:use-foreign-library libawesome)
(cffi:use-foreign-library librad)
```

Now you can trick `cffi` into opening your dynamic libraries instead of default ones by loading
`:awesome-game-libraries` first:

``` common_lisp
;; using asdf
(asdf:load-systems :awesome-game-libraries :awesome-game)
;; or quicklisp
(ql:quickload '(:awesome-game-libraries :awesome-game))
```

Unfortunately, there's other problem that needs to be solved: native libraries can have their
own dependencies we also need to provide. This information is stored in the libraries themselves.

We can use `ldd` tool for GNU/Linux and MSYS2/Windows or `otool -L` command for macOS to list
dependencies of a library. Some dependencies listed there would be system ones and those don't
need to be shipped with a game or an application. Grab non-system libraries from the list and
put them into directory where foreign libraries you planned to ship a game with are
stored. Repeat process for newly copied libraries if required.

Now the only obstacle left is to somehow tell a linker where to find native dependencies we just
copied. One way is to use environment variables, but that would require additional work for our
users, which we try to avoid. Hopefully, it is possible to store relative paths to native
dependencies inside a dynamic library itself.

Windows linker will search for dependencies in the same directory dependent library reside, so
we don't need to do anything special in this case.

For `elf` binaries used by Linux we can use `patchelf` utility to
update [search path](https://en.wikipedia.org/wiki/Rpath) to dependencies in an already compiled
library:

``` sh
patchelf --set-rpath '@ORIGIN/' libawesome.so
```

Now `ld.so` will also search directory where `libawesome.so` lies when looking for its
dependencies.

Process to update `Mach-O` binaries of darwin platform (macOS) is much more involved. Path to
each dependency is actually hard-coded. There's a way
to
[make it relative](https://developer.apple.com/library/content/documentation/DeveloperTools/Conceptual/DynamicLibraries/100-Articles/RunpathDependentLibraries.html),
but needs to be done for each dependency entry registered within a dynamic library:

``` sh
# for each non-system dependency of libawesome.dylib you need to
install_name_tool -change /hard-coded/path/to/libdependency.dylib \
                          @loader_path/libdependency.dylib        \
                          libawesome.dylib
```

### Custom Quicklisp distribution

Asking a user to clone/download a single project into a directory where `asdf` or `quicklisp`
can find it and a user will be able to load it is fine. But if there are few projects to clone,
it quickly becomes cumbersome to do. To partially solve this inconvenience for end-users of our
software, one can deploy custom Quicklisp distribution made with `quickdist` tool[^3] onto
public server somewhere. Users then will be able to load all libraries your game needs with just
two commands:

``` common_lisp
;;; for cl-bodge distribution and :trivial-gamekit system

;; add cl-bodge distribution into quicklisp
(ql-dist:install-dist "http://bodge.borodust.org/dist/org.borodust.bodge.txt")

;; load precompiled native libraries and the gamekit
(ql:quickload '(:bodge-blobs :trivial-gamekit))
```


## Bundle

While distributing one's application amongst fellow Common Lisp users with Quicklisp might seem
like an obvious way and suits libraries especially well, it doesn't really work much for
applications intended for end-users that rarely care about language it was written in. But it is
actually somewhat easier to distribute bundled version of an application rather than its bare
`asdf` system.


### Executable

To build an executable out of a lisp image I would recommend using
Xach's [buildapp](http://www.xach.com/lisp/buildapp/) tool. One important advice for building
stable executables: try to avoid any initializaton during compilation or loading time - put all
of your initialization code into a function and call it in `buildapp`'s `--entry` function you
would need to supply. Starting any threads during loading is especially harmful.


### Dynamic libraries

Because an application should be able to run on different machines, you need to explicitly close
any foreign libraries that are open during loading time before lisp image is dumped into
executable:

``` common_lisp
;; close all loaded foreign libraries
(loop for library in (cffi:list-foreign-libraries :loaded-only t)
   do (cffi:close-foreign-library library))
```

Otherwise, some implementations would try to reload dynamic libraries during image bootstrapping
and, obviously, would fail to do so, because locations of foreign libraries will differ from
machine to machine.

Do not forget to load them back during application initialization:

``` common_lisp
;; reload all foreign libraries
(loop for library in (cffi:list-foreign-libraries)
   do (cffi:load-foreign-library library))
```

To help OS dynamic linker find supplied libraries, this time we can use environment variables
and ask a user to run an application through a shell script.

Example `bash` script for Linux and macOS:

`run.sh`
``` bash
#!/usr/bin/env bash

# find out a directory where .sh lies
WORK_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# if required libraries are stored in the lib/ subdirectory
# relative to .sh script
case $OSTYPE in
    linux*)
        export LD_LIBRARY_PATH="$WORK_DIR/lib/:$LD_LIBRARY_PATH"
        ;;
    darwin*)
        export DYLD_LIBRARY_PATH="$WORK_DIR/lib/:$DYLD_LIBRARY_PATH"
        ;;
esac

cd $WORK_DIR
./awesome-game.bin
```

For Windows's `cmd`:

`run.bat`
``` powershell
@echo off
set WORK_DIR=%~dp0
set PATH=%WORK_DIR%lib\;%PATH%

start /d "%WORK_DIR%" awesome-game.bin
```

### Assets/configuration

Path to assets or any configuration files (that in turn can store any paths itself) we just can
pass as an argument to an executable:

``` bash
./awesome-game.bin awesome-game.conf
```

``` powershell
start /d "%WORK_DIR%" awesome-game.bin awesome-game.conf
```

Then in a function supplied as `--entry` to `buildapp` tool, you will be able to extract all
information you need:

``` common_lisp
;; if we supplied #'main function to --entry option of buildapp
(defun main (args)
  ;; load and parse configuration we passed as an argument to the executable
  (awesome-game:load-configuration (second args))
  (awesome-game:initialize-world))
```

Now put an executable, .sh or .bat script, assets and a configuration file into an archive and
***Ship it!***

***
[^1]: Load into process address space. I use open instead of load here to distinguish between
    foreign libraries loading and asdf system loading

[^2]: Applies to GNU/Linux, Windows and macOS
[^3]: Available in the main Quicklisp distribution
