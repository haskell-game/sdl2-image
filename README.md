# sdl2-image

#### Haskell bindings to SDL_image.

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

Both sets of bindings (both raw and the higher level ones) should allow you to
use any aspect of the original SDL_image library. Please report an issue if you
encounter a problem/bug or feel that something is missing or should be added.
