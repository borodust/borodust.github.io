---
layout: post
title: Guide to cl-bodge graphics system is in the works
excerpt_separator: <!-- excerpt -->
---

Unbeleivably, I've got into writing more `cl-bodge` docs recently. But you know what, now I know
how to make it a lot more fun - literate programming!

Common Lisp, SLY/SLIME and Emacs `org-mode` are especially good for this kind of task.

`org-mode` exposes tools for evaluating code blocks directly from a document and tangling those
blocks into source files appropriately, while Common Lisp + SLY/SLIME combination allows you to
gradually build your project evaluating code bit-by-bit in a live session, meaning you are
writing documentation and building a project at the same time, _meaning_ you are not getting
bored of writing plaintext as much, which is just an amazingly handy trait of writing guides or
examples in literate programming style.

To have a sneak peek into what's coming, you can have a look at the [WIP
guide](https://github.com/borodust/hello-bodge-graphics/blob/master/hello-bodge-graphics.org) on
GitHub. All source code in this project is generated from `.org` files. You can't use the guide
yet, because latest `cl-bodge` updates are not published into `cl-bodge` dist. You can clone
whole `bodge-projects` repository recursively into `~/quicklisp/local-projects/`, but I would
recommend against this practice. At least don't forget to delete all the cloned stuff after
trying it out.
