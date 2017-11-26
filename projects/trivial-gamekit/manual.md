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
activity via certain methods described below.

It strives to fully support Common Lisp's interactive development approach where you change your
application on the fly and allowed to gracefully recover from programming errors via restarts
without crashing or rerunning an application.

## Defining a game

## Math

## Locating resources

## Drawing

## Playing an audio

## Listening to input

## Building a distributable
