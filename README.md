Straightforward library for parsing Nix and pretty-printing.


# Deprecated

*TL;DR*: use [hnix](https://github.com/jwiegley/hnix) instead.

I made this library initially for my [nixfromnpm](https://github.com/adnelson/nixfromnpm) project. However, there is also the library [hnix](https://github.com/jwiegley/hnix), which does mostly the same thing. I, although a bit biased, think there are a few advantages of my library, primarily in terms of simplicity (hence the name) and at least for a time the language coverage was a bit more complete. However, I decided to go with the library that came first and have since contributed a bit to `hnix` to shore it up in certain ways. I also removed its usage from `nixfromnpm` and switched to using `hnix`, and I haven't worked on this library in quite a long time, as you can see. So, although I think there might be some value to this library, and everyone is welcome to fork it or use it as a jumping-off point, I'm deprecating it in favor of `hnix`.
