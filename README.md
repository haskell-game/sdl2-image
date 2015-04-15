# sdl2-image

[![Build Status](https://travis-ci.org/sbidin/sdl2-image.svg?branch=master)](https://travis-ci.org/sbidin/sdl2-image)

#### Haskell bindings to SDL2_image

Both the raw and the higher level bindings should allow you to use any aspect
of the original SDL2_image library. Please report an issue if you encounter a
bug or feel that something is missing.

##### Install

This library depends on and is meant to be used with the `new-api` branch of
[haskell-game/sdl2](https://github.com/haskell-game/sdl2). After installing
haskell-game/sdl2, you can install sdl2-image manually from source:

```bash
git clone git@github.com:sbidin/sdl2-image.git
cd sdl2-image
cabal install
```

Note that you might get compile errors if you're not using the latest GHC. Only
7.10 is currently tested.

##### Documentation

You can find the documentation [here](https://bidin.eu/docs/sdl2-image).

The
[original SDL2_image documentation](http://www.libsdl.org/projects/SDL_image/docs/SDL_image.html)
can also help, as the bindings are close to a direct mapping.

##### Example

A small example executable is included with the library. It loads and displays
a given image. You can find it in the `example` directory.

```bash
cd sdl2-image
cabal run path/to/some/image.type
```
