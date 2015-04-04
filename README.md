# sdl2-image

#### Haskell bindings to SDL_image, both high and low-level.

This library depends on and is meant to be used with the `new-api` branch of
[haskell-game/sdl2](https://github.com/haskell-game/sdl2). Currently you can
only install it manually from source, after installing sdl2 itself.

```bash
git clone git@github.com:sbidin/sdl2-image.git
cd sdl2-image
cabal install
```

A small example executable is included with the library. It loads and displays
a given image. You can find it in the `example` directory.

```bash
cd sdl2-image
cabal run path/to/some/image.type
```
