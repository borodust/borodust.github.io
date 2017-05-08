---
layout: page
title: びの訳
parent: projects
---
A tool for machine-assisted reading from macOS screen.

One day I thought it would be nice to automate part of VN reading by taking partial screenshot
and running it through OCR tool. Like a shell script or something to recognize tricky
kanjis. Few days later I caught myself up on integrating morphological analyzer and translation
service client into tool written in Common Lisp that takes partial screenshot thru macOS native
API, preprocesses the image and feeds it into Tesseract OCR library.

## Links
GitHub repository: [vinoyaku]({{ site.borodust.github }}/vinoyaku)
