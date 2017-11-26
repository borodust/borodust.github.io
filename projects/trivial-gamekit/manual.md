---
layout: page
title: User Manual
project: trivial-gamekit
---

## Contents

* [Overview](#overview)
* [Defining a game](#defining-a-game)
* [Math](#math)
* [Locating resources](#locating-resources)
* [Drawing](#drawing)
* [Playing an audio](#playing-an-audio)
* [Listening to input](#listening-to-input)
* [Building a distributable](#building-a-distributable)


## Overview

`trivial-gamekit` is a very simple toolkit for making games in Common Lisp. Its goal is to be as
simple as possible while allowing a user to make a complete game s/he can distribute.

It manages system resources for you by creating and initializing a window, abstracting away
keyboard and mouse input and any other system-dependent configuration. `trivial-gamekit` also
spawns a default game loop for you allowing to hook into it for drawing and other per frame
activities via certain methods described below.

It strives to fully support Common Lisp's interactive development approach where you change your
application on the fly and allowed to gracefully recover from programming errors via restarts
without crashing or rerunning an application.

## Defining a game

`gamekit`'s central idea is a game object you define via [`defgame`](#gamekit-defgame) macro. It
contains all the state `gamekit` needs to run your game. It is instantiated by
[`#'start`](#gamekit-start) which also bootstraps the whole application and opens a window, runs
your game logic, rendering, etc. Only single game object can be run at a time, so subsequently
calling [`#'start`](#gamekit-start) for the same or different game objects is disallowed - you
need to stop game execution. To stop a running game you would need to call
[`#'stop`](#gamekit-stop).

### Related symbols

{% include_relative defining-a-game.md %}


## Math

## Locating resources

## Drawing

## Playing an audio

## Listening to input

## Building a distributable
