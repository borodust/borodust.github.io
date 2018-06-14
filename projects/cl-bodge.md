---
layout: page
title: cl-bodge
parent: projects
---

1. [Introduction](#introduction)
1. [Sources](#sources)
1. [Installation](#installation)
1. [Related Projects](#related-projects)
1. [Support](#support)


## Introduction

`cl-bodge` is a cross-platform Common Lisp application framework tailored for game development
needs. It unites diverse libraries and OS facilities under a single consistent interface and,
consequently, smoothes interoperability across different subsystems: resource handling,
graphics, audio, physics, GUI and many other aspects of an application or, ultimately, a game.


`cl-bodge` is highly modular and extensible. It is split into several subsystems and you are
free to use only what is required for your particular needs or you can write a subsystem
yourself!


## Installation

### Via [Quicklisp](http://quicklisp.org)

```common_lisp
;; add cl-bodge distribution into quicklisp
(ql-dist:install-dist "http://bodge.borodust.org/dist/org.borodust.bodge.txt" :replace t :prompt nil)

;; load cl-bodge demo
(ql:quickload :cl-bodge/demo)

;; start the demo
(cl-bodge.demo:run)
```


## Sources
GitHub repository: [cl-bodge]({{ site.borodust.github }}/cl-bodge)

## Related Projects
[trivial-gamekit]({{ site.borodust.github }}/trivial-gamekit)
: Substantially simplified interface to `cl-bodge` for quick introduction into gamedev with
  Common Lisp

[bodge-appkit]({{ site.borodust.github }}/bodge-appkit)
: System for quick `cl-bodge` application bootstrapping.


[cludge]({{ site.borodust.github }}/cludge)
: Visual editor for `cl-bodge`


## Support
`cl-bodge` is an experimental one-man project so far - things could break. And if they do, feel
free to contact me via [email](mailto:dev@borodust.org) or find me and awesome `#lispgames`
community chatting on [`#lispgames`](https://webchat.freenode.net/?channels=lispgames) channel at
`irc://chat.freenode.net`.
