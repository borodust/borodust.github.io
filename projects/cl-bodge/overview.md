---
layout: page
title: Overview of cl-bodge
project: cl-bodge
---

As mentioned in [Introduction]({% link projects/cl-bodge.md %}), `cl-bodge` is a
[modular](#modular), [extensible](#extensible), [cross-platform](#cross-platform) [application
framework](#application-framework) written in [Common
Lisp](https://en.wikipedia.org/wiki/Common_Lisp).


## Modular

`cl-bodge` is literally just a group of systems sharing the same interface and built upon the
same core: facilities provided by `cl-bodge/engine`. One of the initial goals of writing
`cl-bodge` was to unite diverse libraries (CL and C alike) under a single interface
umbrella. `cl-bodge/engine` provides all required primitives that enables all dependent systems
to have outstanding interoperability: memory management, concurrency and thread-safety, external
resource handling, event-driven communication, linear algebra, application bootstrapping,
lifecycle management and other primitives for building interoperable systems.

You are not required to use any of `cl-bodge` subsystems apart from `cl-bodge/engine`. Want to
build your own engine from scratch, but lazy enough to wrap all the basic stuff yourself? Sure!
Just use `cl-bodge/engine` as a fundation. Maybe the only thing you need is access to host input
and window? Sure! Only add `cl-bodge/host` to your dependencies and nothing else of `cl-bodge`
will be loaded keeping your CL image clean of unnecessary bits. Would like to use physics? No
probs here either! `cl-bodge/physics` is at your disposal. Now, if you want to work with 2D
graphics you add `cl-bodge/canvas` as a dependency, but that would also bring `cl-bodge/host`
and `cl-bodge/graphics` too, because `cl-bodge/canvas` depends on them itself: some `cl-bodge`
subsystems do depend on others.

`cl-bodge` modules:
* `cl-bodge/engine` - provides primitives for memory management, concurrency, math, event
processing, resource processing and application lifecycle management. `cl-bodge` core library.
* `cl-bodge/resources` - library for external resource handling (audio, images, 3d models,
  animation, etc)
* `cl-bodge/host` - abstraction layer over host OS-dependent functionality: windows, OS resource
  management, input, OS events
* `cl-bodge/graphics`- 3d rendering primitives
* `cl-bodge/canvas` - routines for 2d drawing and vector graphics (hardware-accelrated)
* `cl-bodge/animation` - support module for skeletal animation: keyframe sequences, keyframe
  interpolation, skinning shader
* `cl-bodge/audio` - module for handling positional 3d audio
* `cl-bodge/physics` - 2D and 3D rigid body physics module with collision detection
* `cl-bodge/ui` - module for creating in-app (non-native) hardware-accelerated multi-window user
  interfaces
* `cl-bodge/text` - SDF-based text rendering
* `cl-bodge/distribution` - utilities for packaging application for shipping (delivery).


## Extensible

You can define subsystems yourself and they would be as good as any other `cl-bodge`
ones. `cl-bodge/engine` exposes several classes and methods to accomplish this. Once you follow
those interfaces your system would interact with anything else in `cl-bodge` just like any other
native subsystems interact between themselves.


## Cross Platform

Under the hood, `cl-bodge` framework is not a pure Common Lisp and that was never a goal to make
it so. It reuses as much as possible from CL ecosystem, but if there's no appropriate library or
interface available, it includes foreign code with C ABI compatible interface internally[^1]. But
all of this is done in a cross-platform fasion - you can run `cl-bodge` and all its subsystems
(input, physics, audio, graphics, etc) across 3 major OS platforms (GNU/Linux, Windows, macOS)
and several CL implementations (SBCL, CCL, ECL) without any changes to the source code.

## Application Framework

`cl-bodge` is somewhere on the edge of already blurry border between application framework and a
game engine. It has its own application lifecycle management and a couple of requirements to
make subsystems interoperable, but otherwise it doesn't impose any strict rules on how your
application should work - you are fully in control of it. If you wish so, you don't even need to
integrate with `cl-bodge` lifecycle whatsover. Or you can build a full-blown game engine on top
of `cl-bodge` and expose only a couple of entry points via
[`ECS`](https://en.wikipedia.org/wiki/Entity-component-system) and scripting.


[^1]: `cl-bodge` is pure Common Lisp on the outside though! Its API strives to be consistent
    with ecosystem standards.
