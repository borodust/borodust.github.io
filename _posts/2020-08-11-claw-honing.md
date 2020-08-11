---
layout: post
title: :claw honing - First milestone
excerpt_separator: <div id="excerpt"></div>
---

Huge first milestone for `:claw` honing is reached - I've run C++ library
routines ([PhysX](https://github.com/NVIDIAGameWorks/PhysX)) from Common Lisp
without writing any `C/C++` myself, without writing any bindings
manually. `:claw` generated `C` adapter and `CL` bindings, I only built the former
and called the latter.

This is an important proof of the concept:
* `IFFI` approach works
* C wrapper generation for C++ is possible with `libresect` (`libclang`) - no
need to dive into llvm/clang development libraries any deeper, or so it seems so
far.

99% work done, so only another 99% left.

<div id="excerpt"></div>


## Wait.. PhysX?

I've switched from `Filament` to `PhysX` wrapping, because the latter one
requires much less work to get it actually running. `Filament` comes after.

## What's next

Next I'll slowly start bringing `:claw` level of C++ wrapping support to that of
C while adding any missing C++ features I'll stumble upon. Then polishing and
code cleanup - it's got a bit messy in the process. Hopefully, I won't be burnt
out by then completely and will be able to add proper documentation, so anyone
else would be able to enjoy `:claw` ~~bugs~~features.

I also decided to add system generation capability to `:claw`. At the moment, it
works similar way `cl-autowrap` operates - you have primary macro with
configuration that expands into bindings. For that to work, you need to load
whole `:claw` bundle and that's totally an overkill - you don't need any parsing
and autogeneration when only thing you want is to load pregenerated bindings. So
`:claw` will have an option to generate an ASDF system for you to do just that -
load bindings for the architecture of your choice.

Basically, you would use system with primary macro to generate bindings and an
ASDF system to load them, then in your project that needs those bindings you
would depend directly on this generated system rather than on `:claw`.

`:claw` is performance-oriented first and it also cares about image size of your
application.
