---
layout: post
title: April's 2017 Lisp Game Jam Postmortem
permalink: 2017-04-lgj-postmortem
excerpt_separator: <!--excerpt-->
---

That was a blast! And so [Mortar Combat](https://borodust.itch.io/mortar-combat) was born.

I probably should have stopped right here, but, apparently, there quite a few people interested
in the events of those days. Their minds call them to fill up the void made by vicious curiosity
that delves deeper and deeper to clear space for pure knowledge of what actually happened during
the sacred ritual known as [Lisp Game Jam](https://itch.io/jam/lisp-game-jam-2017-easy-mode)
where miraculous [Mortar Combat](https://itch.io/jam/lisp-game-jam-2017-easy-mode/rate/134760)
was bor... ---Nah, right, you got me there--- No one cares, but I'm still gonna put some words
about it here nevertheless.

* [Preparations](2017-04-lgj-postmortem#preparations)
  * [Games, Socks and Decision](2017-04-lgj-postmortem#games-socks-and-decision)
  * [Realization](2017-04-lgj-postmortem#realization)
  * [Plans](2017-04-lgj-postmortem#plans)
* [DevLog](2017-04-lgj-postmortem#devlog)
* [Conclusion](2017-04-lgj-postmortem#conclusion)

<!--excerpt-->


## Preparations

### Games, Socks and Decision

Lets be honest up front: I cook socks as a game designer. Doubt never crossed my mind about this
fact. Why do even bother with a game engine then, you might ask. Exactly because, would be my
answer. But really I'm just working on a tool that helps me to learn magic of virtual
worlds. Exploratory programming they call it.

And so that was it. I decided to make a "game", more of a tech demo, as complex as the engine
would allow me to finish in time while adding minor missing components I could incorporate and
reuse later. Current goal is VR+networking so it was sort of no brainer when I decided to go
with Multiplayer 3D First Person Shooter™.


### Realization

It was a Friday's afternoon. Few lazy hours left till the Jam. Suddenly, belated realization
came to me as to awake me gently: ~~how the fuck~~ holy duck, how did I forget I actually could
prepare goddamn assets for a Jam game in advance. That's how I lost a day or two of Jam before
it even started.

No need to mention that socks I cook as an artist are so huge, Big Foot dudes use them as race
sacks. Hopefully, I fairly quickly
found [free model](https://free3d.com/3d-model/rigged-stick-figure-by-swp-2-55987.html) I felt
like using as a "Dude" to portray combatants, but I failed hard to find simple *animated*
human-like skeleton to transfer onto this model[^1]. My only luck was an IK-driven skeleton
from [different model](http://www.blendswap.com/blends/view/45969) I successfuly imported into
Blender. So I merged those into one and the model was ready to be animated.


### Plans

I attacked the available timeline by figuring out what features I would need the engine lacks
yet. Knowing out of experience from the previous Jam
that [shipping Common Lisp games]({{ site.baseurl }}{% post_url 2017-05-02-lisp-delivery %}) is
not a trivial task these days, I thoroughly prepared a delivery mechanism before this Jam
started and tested it with help from [`#lispgames`](http://lispgames.org) community
beforehand. So, ultimately, distribution part was out of concern.

What was missing:
* Animation blending
* Any sort of networking
  * Game client
  * Game server
  * Communication server (to avoid NAT problems)
  * Matchmaking
  * Packet delay smoothing
* Ragdolls

Last feature was an extra, because at this point I knew I didn't have time to actually make a
model ragdolled, because I don't have a tool that would allow me to apply rigid bodies and
joints to a skeleton in WYSIWYG-manner[^2] and doing that manually by hand is much
pita. But I thought I would have time to at least use single rigid body for cringy ragdolled
death animation. I was wrong, but meh, extra is an extra: no regrets.


## DevLog

### Apr 15, 2017
#### [8a77603](https://github.com/borodust/mortar-combat/commit/8a77603) Initial commit

Many of us start here. Conventional copy-pasting from other projects. One day
I'll [fix that](https://github.com/borodust/cl-bodge/issues/74).

#### [7d49cae](https://github.com/borodust/mortar-combat/commit/7d49cae) Distribution skeleton

I had to make sure that distribution subsystem still functions properly, so I decided to check
it up early in the process. I've got bare window, context and systems initialization all working
alongside functional distribution. Mild success, but huge relief - there's already something
that other people can actually try and see.

#### [f03e304](https://github.com/borodust/mortar-combat/commit/f03e304) Dude
#### [0003b9d](https://github.com/borodust/mortar-combat/commit/0003b9d) Running dude

Someone mentioned at `#lispgames` channel that there's a
forum [thread](https://itch.io/jam/lisp-game-jam-2017-easy-mode/topic/72887/screenshots) already
to show the progression.

> "Dammit! I need to demonstrate something too."

At this point I already learned[^3] how to do animations in Blender and made a crappy running
cycle, so the next goal was to bring the result of my Blender madskillz into game viewport
asap. So [I did](https://itch.io/post/147009).


### Apr 16, 2017
#### [01beb4d](https://github.com/borodust/mortar-combat/commit/01beb4d) Strafing dude

Progression thread continued to fill up with drafts and screenshots. It was one of the driving
forces behind decision making I did to schedule next goals for a while. I needed more
visuals. So, obviously, [strafing](https://media.giphy.com/media/3oKIPdDFcuQIdoNWQE/giphy.gif)
came next.

#### [4262e0b](https://github.com/borodust/mortar-combat/commit/4262e0b) Dudes and mortars

People started wondering what the hell is going to be in the hands of those dudes? Babies? So I
thought it is time to show
the [weapon model](https://www.cgtrader.com/free-3d-models/military/gun/spanish-mortar-1747) I
prepared. Found and prepared.

This day I learned: Never forget to apply goddamn global model transformations before exporting.

I spent a few hours figuring out why exported mortar model is totally messed up after loading
into the engine.  I thought topology might be broken somewhere, so the polygons or winding got
screwed during export.  And I indeed found topology problems. Now, it is probably good to know
how to fix broken topology in Blender and knowledge of non-manifold geometry existence is
helpful, but only after I learned all that crap I realized.. ~~Shit!~~ Shading! It was just
incorrectly lighted. Shading is totally a culprit here. Shaders were sort of the same I used for
dudes, so problem lied somewhere else.

Oh, yes, it did. First, apparently, model had negative scale so all normals and faces came out
inverted. Second, holy ~~crap!~~ crop, scaling was set to 0.28 or smth for all
axes. *---Eh?!---* But at least model had correct topology now... gaawddammit.

[Show off](https://itch.io/post/147753) time!

#### [3c90417](https://github.com/borodust/mortar-combat/commit/3c90417) Ball

Well, I needed it. I even made it myself. I still am able to click a proper button, hopefully.

### Apr 17, 2017
#### [369ad98](https://github.com/borodust/mortar-combat/commit/369ad98) Room
#### [78d757c](https://github.com/borodust/mortar-combat/commit/78d757c) Split into client and proxy server

Having smth to show is nice, but there was quite a lot of work to do that no one will ever see
and it would be taken for granted: networking. Still, I was excited: I hardly wrote much of
real-time network communication code - mostly REST stuff. Now I finally had a solid reason to
implement it.

### Apr 18, 2017
#### [bbf0592](https://github.com/borodust/mortar-combat/commit/bbf0592) Add proxy routing stub
#### [4b9fc4c](https://github.com/borodust/mortar-combat/commit/4b9fc4c) Add peer registry
#### [130fc99](https://github.com/borodust/mortar-combat/commit/130fc99) Add identify command
#### [c66234e](https://github.com/borodust/mortar-combat/commit/c66234e) Async networking with cl-flow

Server instance I had to serve as a medium for game communications was not particularly
performat one: single-core 2GHz with 512Mb RAM. Not much resources to
spare. Blocking/synchronous approach wasn't much of an option, so I went straight with
asynchronous code.

Asynchronous approach was especially attractive, because I could and did integrate it nicely
with [cl-flow](https://github.com/borodust/cl-flow) library. Success!

#### [248ae83](https://github.com/borodust/mortar-combat/commit/248ae83) Arena commands
#### [d756789](https://github.com/borodust/mortar-combat/commit/d756789) Add register-game-stream command

Remote server is just a medium to help players find each other and to establish communication
channel between them. So I decided to go with two connections per peer: one would serve as a
channel between communication server and a peer, and the other connection is for data stream to
be routed between peers without any parsing done by communication server to preserve resources.
So, basically, server<->peer and peer<->peer connections.

### Apr 19, 2017
#### [d64ff70](https://github.com/borodust/mortar-combat/commit/d64ff70) Extract common code between client and server
#### [232f276](https://github.com/borodust/mortar-combat/commit/232f276) Add ping command

That was a moment of truth. First pings sent across peers and a server! Too bad there was no
entertaining way to post such a progress into screenshot thread :/

#### [1ad1d04](https://github.com/borodust/mortar-combat/commit/1ad1d04) Force output pouring
#### [e8a41e5](https://github.com/borodust/mortar-combat/commit/e8a41e5) Switch from usocket to cl-async

Unfortunately, [usocket](https://github.com/usocket/usocket)[^4] didn't allow me to write fully
asynchronous code (efficient reading was a problem: read-sequence is still a blocking
operation), so I rewrote communication server
using [cl-async](https://github.com/orthecreedence/cl-async) networking capabilities. That
didn't give me any particular troubles. Moreso, I was happy with it.

#### [f9948c7](https://github.com/borodust/mortar-combat/commit/f9948c7) Add LGJ mention
#### [cea602a](https://github.com/borodust/mortar-combat/commit/cea602a) Rest pose
#### [750002a](https://github.com/borodust/mortar-combat/commit/750002a) Add animation blending

Adding animation blending was the next huge milestone. Interpolation part was already there, so
all I had to do was a bit of refactoring and adjusting a few interfaces to introduce blending
into the world.

[New visuals](https://itch.io/post/150303) were ready!


### Apr 20, 2017
#### [fc34a5b](https://github.com/borodust/mortar-combat/commit/fc34a5b) Add keymap
#### [5a1adeb](https://github.com/borodust/mortar-combat/commit/5a1adeb) Player camera and movement
#### [f23d6d9](https://github.com/borodust/mortar-combat/commit/f23d6d9) Ball body

By this time I slowly got to the point where I should be able to move around to test upcoming
features.

### Apr 21, 2017
#### [c0207d3](https://github.com/borodust/mortar-combat/commit/c0207d3) Add shooting
#### [3a8cd03](https://github.com/borodust/mortar-combat/commit/3a8cd03) Dude body
#### [723fe65](https://github.com/borodust/mortar-combat/commit/723fe65) Add room mesh wall geom

Physics simulation was finally ready to
be [demonstrated](https://itch.io/post/151372). Basically, at this point, it was possible to
turn it into some proper game with distinct goals and visuals, but without any networking. Maybe
some sort of aim'n'shoot stuff like shooting gallery or whatevs. But it still would ended up
crappy, because I was in charge, but I never actually considered pivoting it into something else
anyway :)


#### [d0cfc8f](https://github.com/borodust/mortar-combat/commit/d0cfc8f) Add basic communication
#### [29a4b8f](https://github.com/borodust/mortar-combat/commit/29a4b8f) Game server and client

### Apr 22, 2017
#### [b5fa9e1](https://github.com/borodust/mortar-combat/commit/b5fa9e1) Clean up peer data on disconnect
#### [687a95e](https://github.com/borodust/mortar-combat/commit/687a95e) Add :exit-arena message
#### [5311d42](https://github.com/borodust/mortar-combat/commit/5311d42) Add Makefile and Dockerfile for building the server

It was the time communication server should have been stabilized to the point when I could just
deploy it and focus on game server and game client code. I already used Docker to deploy several
backends including lisp ones, so going that route was an obvious choice. No obstacles met.
Except, maybe, that Alpine Linux actualy uses `musl-libc` instead of `glibc`, meaning I couldn't
deploy systems built on my Arch box for it as a base image. Meh, but no worries: we have
officially supported bloated Ubuntu base image. Yays for 150Mb of bloat.

#### [3f0762c](https://github.com/borodust/mortar-combat/commit/3f0762c) Add naive position smoothing

Well, as expected, roundtrip time through remote communication server was quite crappy:
something about 0.125s. I needed smoothing so movement of the players would look acceptable. I
tried to implement naive prediction, but that didn't go well, so I just went with "last packed
interpolation". Basically, players were one packet behind their actual positions reported to
game server. But the end result was actually very nice: only perceivable lag was actually
network one. Arguably, smoothing didn't contribute any visible lag.


### Apr 23, 2017
#### [af8617a](https://github.com/borodust/mortar-combat/commit/af8617a) Add arena object
#### [b681885](https://github.com/borodust/mortar-combat/commit/b681885) Send game updates every couple of frames
#### [91065e3](https://github.com/borodust/mortar-combat/commit/91065e3) Add movement state
#### [c05401c](https://github.com/borodust/mortar-combat/commit/c05401c) Relay movement state
#### [d980404](https://github.com/borodust/mortar-combat/commit/d980404) Add movement animation decorators

I felt too lazy to make 8 distinct animations to introduce backward running, other way strafing
and everything in between. So after some tinkering I actually ended up with animation player and
animation decoration mechanism. Now I could combine several animation sequences into one by
decorating them with "modifiers": looping, backward playing, blending. So now I could do 8-way
movements out of running and strafing animations with just a couple lines of code. Wows. Result
was crappy and even creepy, but wows.

#### [e395466](https://github.com/borodust/mortar-combat/commit/e395466) Add client/server shot handling
#### [4d626a1](https://github.com/borodust/mortar-combat/commit/4d626a1) Add login window
#### [bbf5d6f](https://github.com/borodust/mortar-combat/commit/bbf5d6f) Add base menus
#### [7cdabeb](https://github.com/borodust/mortar-combat/commit/7cdabeb) Add arena creation and joining to GUI
#### [7925fdb](https://github.com/borodust/mortar-combat/commit/7925fdb) Toggle game menu
#### [9179e6b](https://github.com/borodust/mortar-combat/commit/9179e6b) Add score table

GUI was known to work, but I met very interesting bug while implementing a couple of menus.

If you look into the sources, one of the button titles is `"Refresh "`. This whitespace in the end
is not a typo. Apparently, if I name it `"Refresh"` any attempt to close an in-game window will
hang a whole GUI. Yes. After a couple of "no really, wtf" moments I decided not to waste time
investigating. I would have plenty of
time [to do so](https://github.com/borodust/cl-bodge/issues/76) post-Jam.

#### [11eab86](https://github.com/borodust/mortar-combat/commit/11eab86) Register when server player is hit

### Apr 24, 2017
#### [f5dda29](https://github.com/borodust/mortar-combat/commit/f5dda29) Cap physics step
#### [c279bc8](https://github.com/borodust/mortar-combat/commit/c279bc8) Disable camera before start
#### [055b8d6](https://github.com/borodust/mortar-combat/commit/055b8d6) Add arena leaving

Mostly minor stuff left so I devoted time to plug most obvious holes. Unfortunately, at this
point it was clear I wouldn't have time to add any sounds into the game or any, even crappy,
ragdolls or death animations. I barely had time to introduce at least some goal into the game
(Score table). And even then, it was bugged: it counted how many times player was hit, instead
of how many times player has hit anyone :/

#### [36ad117](https://github.com/borodust/mortar-combat/commit/36ad117) Add dude and mortar model sources

Obligatory asset sources. I probably should have added them to the repository from the start,
but didn't do so for some reason I can't remember myself.


## Conclusion

A blast indeed! Native, shippable across platforms, complex 3D tech demo with smooth multiplayer
in 10 days - Achievement unlocked!

Yes, exhausted by the end, but I reached all the planned goals, except for optional
ragdolling. New features are ready to be incorporated
into [the engine](https://github.com/borodust/cl-bodge/) and I learned several ways in which it
could be improved.

Big thanks to all participants and especially to Michael [`axion`](https://itch.io/profile/axion) Fiano for organizing such an event. It was fun!

P.S. Woohoo, Mortar Combat didn't even get
last [place](https://itch.io/jam/lisp-game-jam-2017-easy-mode/results), but 4th. Wows!

---------
[^1]: Skeleton of which I couldn't get to import into Blender correctly. Sigh.

[^2]: Blender actually can, but I don't have an exporter/importer that would allow to bring that
    information into the game engine.

[^3]: By means of whatever [lesson](https://www.youtube.com/watch?v=kSDWfx6ib9k) popped up first
    in search results

[^4]: Relevant [issue](https://github.com/usocket/usocket/issues/12)
