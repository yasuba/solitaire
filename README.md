# Solitaire

A Klondike solitaire game built with Scala 3 and [Indigo](https://indigoengine.io/), compiled to JavaScript via Scala.js.

**Play it here:** https://yasuba.github.io/solitaire/

## Features

- Drag-and-drop and tap-to-move card interactions
- Works on desktop and mobile browsers
- Undo support
- Auto-complete when all cards are face-up
- Timer tracking

## Build

Requires sbt and Java.

```
sbt fullLinkJS
cp target/scala-3.4.1/solitaire-opt/main.js main.js
```