---
layout: page
title: Advanced Features
project: trivial-gamekit
---

## Contents
* [Continuous Integration and Deployment](#continuous-integration-and-deployment)
* [Going deeper with `cl-bodge`](#going-deeper-with-cl-bodge)


## Continuous Integration and Deployment

### Introduction

Services like [Travis CI](https://travis-ci.org) and [AppVeyor](https://appveyor.com/) allow us
to apply continuous integration practices to Common Lisp software across 3 major platforms:
`Windows` (AppVeyor), `GNU/Linux` and `macOS` (Travis CI). Not only pure CI, but it is also
possible to deploy built software to several providers including [GitHub](https://github.com/)
releases. `trivial-gamekit` is shaped with CI and cross-platform building in
mind. Unfortunately, I'm not going to explain how to use those services in great detail, but
only the bits required to build software based on `trivial-gamekit`.

Lets see what has to be done to use those tools with GitHub.


### Travis CI
If you don't have Travis CI account yet, go to its [website](https://travis-ci.org) and follow
registration process. Setup Travis CI to track your project's GitHub repository.


Put this snippet into `.travis.yml` at the root directory of your project:

```yaml
language: common-lisp
sudo: false

addons:
  apt:
    packages:
    - zip

env:
  global:
  - GAMEKIT_SYSTEM_NAME: example
  - GAMEKIT_APPLICATION_PACKAGE: example-package
  - GAMEKIT_APPLICATION_MAIN_CLASS: example
  - PATH: ~/bin/:$PATH
  - GAMEKIT_TARGET_PACKAGE: $GAMEKIT_SYSTEM_NAME-x86-64-$TRAVIS_OS_NAME-$TRAVIS_BRANCH.zip
  - GAMEKIT_BUILD_DIR: /tmp/$(GAMEKIT_SYSTEM_NAME)
  - secure: "your+encrypted+data+containing+GITHUB_TOKEN=githubaccesstoken"

branches:
  only:
    - "/^v\\d+(\\.\\d+)+$/"

os:
  - linux
  - osx

install:
  - curl -L http://bodge.borodust.org/files/install.sh | sh

cache:
  directories:
  - "$HOME/quicklisp/"
  - "$HOME/opt/sbcl/"
  - "$HOME/.config/common-lisp/"

script:
  - >
    sbcl --script $HOME/bodge/scripts/build-gamekit-system.lisp
    $GAMEKIT_SYSTEM_NAME $GAMEKIT_APPLICATION_PACKAGE $GAMEKIT_APPLICATION_MAIN_CLASS
    $TRAVIS_BUILD_DIR
    $GAMEKIT_BUILD_DIR

before_deploy:
  - mv "$GAMEKIT_BUILD_DIR/$GAMEKIT_SYSTEM_NAME.zip" $GAMEKIT_TARGET_PACKAGE

deploy:
  provider: releases
  api-key: $GITHUB_TOKEN
  file: $GAMEKIT_TARGET_PACKAGE
  skip_cleanup: true
  overwrite: true
  on:
    tags: true
```

First, you need to generate private GitHub [access token](https://github.com/settings/tokens),
encrypt it and let travis know about it, so it could deploy prepared packages to your
repository. Here's the [guide](https://docs.travis-ci.com/user/encryption-keys/) explaining how
to encrypt your token. Encryption command would look something like this:

```sh
travis encrypt GITHUB_TOKEN="yourgeneratedgithubtoken"
```

Put encrypted string into `env.global` section of `.travis.yml` (See `secure` value above).
Replace `example` value in `GAMEKIT_SYSTEM_NAME` with actual name of `asdf` system of your
project. Put package name that contains your main class (defined with `gamekit:defgame`) into
`GAMEKIT_APPLICATION_PACKAGE` and name of this class into `GAMEKIT_APPLICATION_MAIN_CLASS`
variable. That should be enough to build the project!


This `.travis.yml` is setup to build a project on tag push. Tag name should use `v` +
version-triplet format, e.g.`v1.0.0`.

Commit changes and push to your repo:

```sh
git add .
git commit -m "Travis CI configuration"
git tag v1.0.0
git push origin v1.0.0
```

Travis CI should pick up the changes, start to build your project and then deploy resulting package
into GitHub releases of your project repository under `v1.0.0` tag.


### AppVeyor

While `Travis CI` allows you to build `GNU/Linux` and `macOS` version of your software, it
doesn't support windows yet. Hopefully, AppVeyor does!

Go to AppVeyor [website](https://travis-ci.org) and follow their registration process that is
straightforward as Travis CI one. Setup AppVeyor to track your project's GitHub repository in
similar to Travis CI manner.

Put this snippet into `.appveyor.yml` at the root directory of your project:

```yml
image:
  - Visual Studio 2017

platform:
  - x64

environment:
  global:
    GAMEKIT_SYSTEM_NAME: example
    GAMEKIT_APPLICATION_PACKAGE: example-package
    GAMEKIT_APPLICATION_MAIN_CLASS: example
    GAMEKIT_ARTIFACT: $(GAMEKIT_SYSTEM_NAME)-x86-64-windows-$(APPVEYOR_REPO_TAG_NAME).zip
    GAMEKIT_BUILD_DIR: $(TMP)\$(GAMEKIT_SYSTEM_NAME)

skip_non_tags: true

branches:
  only:
    - master
    - "/^v\\d+(\\.\\d+)+$/"

install:
  - set PATH=C:\msys64\usr\bin\;%PATH%
  - pacman --noconfirm -S zip
  - sh -c "curl -L http://bodge.borodust.org/files/install.sh | sh"

build_script:
  - >
    sh -c "$HOME/bin/sbcl --script $HOME/bodge/scripts/build-gamekit-system.lisp
    %GAMEKIT_SYSTEM_NAME% %GAMEKIT_APPLICATION_PACKAGE% %GAMEKIT_APPLICATION_MAIN_CLASS%
    $(cygpath -u '%APPVEYOR_BUILD_FOLDER%')
    $(cygpath -u '%GAMEKIT_BUILD_DIR%')"
  - mv %GAMEKIT_BUILD_DIR%\%GAMEKIT_SYSTEM_NAME%.zip %GAMEKIT_ARTIFACT%

artifacts:
  - path: "%GAMEKIT_ARTIFACT%"
    name: release_archive

deploy:
  provider: GitHub
  release: $(APPVEYOR_REPO_TAG_NAME)
  tag: $(APPVEYOR_REPO_TAG_NAME)
  description: $(APPVEYOR_REPO_COMMIT_MESSAGE)
  auth_token:
    secure: "yourencryptedgithubkey"
  artifact: release_archive
  force_update: true
  draft: false
  prerelease: false
  on:
    appveyor_repo_tag: true
```

Just like with Travis CI, you would need to generate private GitHub [access
token](https://github.com/settings/tokens), encrypt it via AppVeyor [encryption
page](https://ci.appveyor.com/tools/encrypt) putting there your new access token and replace
`secure` value of `deploy` section of the snippet above with an actual value.  Same as with
`.tavis.yml`, put package name that contains your main class (defined with `gamekit:defgame`)
into `GAMEKIT_APPLICATION_PACKAGE` and name of this class into `GAMEKIT_APPLICATION_MAIN_CLASS`
variable.  Again, just like Travis CI config, this `.appveyor.yml` is setup to build a project
on tag push. Tag name should use `v` + version-triplet format, e.g.`v1.0.0`.

Commit changes and push new tag to project origin:

```sh
git add .
git commit -m "AppVeyor CI configuration"
git tag v1.0.1
git push origin v1.0.1
```

Binary package for `Windows` with `v1.0.1` version of your project should be deployed to its
GitHub repository after successful build!


## Going deeper with `cl-bodge`

`trivial-gamekit` is really just a wrapper around vast
[`cl-bodge`](https://github.com/borodust/cl-bodge) API. For example, almost all `draw-`
functions are really just a reexported symbols from the canvas subsytem of `cl-bodge`. Actually,
if you look into sources of the `gamekit`, you will see, that it is itself just another system
of `cl-bodge`! That means, once you feel `gamekit` features are no longer enough to realize your
ideas, you can start using `cl-bodge` API right away. Keep in mind though: while
`trivial-gamekit` is considered stable, `cl-bodge` itself is a work in progress. API of the
latter is a subject to change, unlike `gamekit`'s one, which is promised to stay stable.
