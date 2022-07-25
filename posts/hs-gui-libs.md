---
title: The Big List of Haskell GUI Libraries
date: 2022-07-03
modified: 2022-07-25
---

*(Note: this originated as a list
  [I](https://www.reddit.com/r/haskell/comments/a6bvr2/graphics_in_haskell/ebxti0v/)
  [kept](https://www.reddit.com/r/haskell/comments/efylna/haskell_gui/fc3xh18/?utm_source=reddit&utm_medium=web2x&context=3)
  [on](https://www.reddit.com/r/haskell/comments/efylna/haskell_gui/fc3xh18/?utm_source=reddit&utm_medium=web2x&context=3)
  [reposting](https://www.reddit.com/r/haskell/comments/q3747e/cant_install_wxhaskell_on_windows/hfs8s25/?context=3)
  in response to repeated questions on /r/haskell, and more recently on the Discord FP server too.
I figured it was finally time to give it a permanant home.
I’ve also taken the opportunity to expand it out a little bit.)*

Haskell has an excellent selection of GUI libraries, both for desktop and for web applications.
However, choosing between them can be difficult.
In practice I find the main differentiator to be ease of installation:
  some Haskell GUI libraries are notably difficult to install, but others are much easier to get working.
They also differ strongly in look and feel, as well as in programming paradigm
  (imperative vs [FRP](https://wiki.haskell.org/Functional_Reactive_Programming) vs [Elm architecture](https://guide.elm-lang.org/architecture/)).
This article aims to list the GUI libraries available for Haskell and give an outline of their differences.

<!--more-->

**Recommendations:** [`gi-gtk`](https://hackage.haskell.org/package/gi-gtk) is probably the best all-round desktop GUI library for Haskell, if you can get it installed.
If not, [`fltkhs`](https://hackage.haskell.org/package/fltkhs) and [`monomer`](https://github.com/fjvallarino/monomer) should also work well.
For more advanced usecases, Haskell backends [can be linked](https://github.com/bradrn/brassica/blob/73f55b6707289944ae20a15cde135294d4f7c4b6/ARCHITECTURE.md)
  to a frontend GUI written in C or C++.
For web GUIs, try [`threepenny-gui`](https://hackage.haskell.org/package/threepenny-gui), [Miso](https://github.com/dmjio/miso/blob/master/README.md) or [Reflex](https://reflex-frp.org/) —
  the former is much easier to both use and install, but the latter two allow compilation to JavaScript via GHCJS.

## Desktop GUI libraries

Probably the best all-round desktop GUI library for Haskell is [`gi-gtk`](https://hackage.haskell.org/package/gi-gtk), which provides Haskell bindings to the widely-used [GTK+](https://www.gtk.org/) library.
`gi-gtk` is easy to use, and is documented fairly well by the [official GTK+ documentation](https://www.gtk.org/docs/) for C and other languages.
The only major problem with `gi-gtk` is that it can be difficult to install on Windows, despite the existence of an [installation guide](https://github.com/haskell-gi/haskell-gi/wiki/Using-haskell-gi-in-Windows).
Additionally, GTK+ can look and feel conspicuously non-native, especially on Windows.

(Sometimes the older libraries `gtk3` and `gtk2hs` are also mentioned.
However, those are currently unmaintained, and `gi-gtk` should be used instead.)

Also worth mentioning are the two libraries [`gi-gtk-declarative`](https://owickstrom.github.io/gi-gtk-declarative/)
  and [`gi-gtk-declarative-app-simple`](https://owickstrom.github.io/gi-gtk-declarative/app-simple/).
These are not standalone libraries, but build on `gi-gtk` to provide a less imperative interface.
`gi-gtk-declarative` defines the underlying data structures and functions required to build a declarative GUI framework.
One suce framework built on `gi-gtk-declarative` is `gi-gtk-declarative-app-simple`, which provides a simple declarative framework based on the Elm architecture.
For smaller GUIs, `gi-gtk-declarative-app-simple` is thus very suitable.
However, for more complex applications it can be easier to write a custom `gi-gtk-declarative`-based framework.
Personally, I prefer to simply use `gi-gtk` directly, but others may prefer to use such a declarative framework.

As well as GTK+ bindings, there are Haskell bindings to [FLTK](https://www.fltk.org/), in the form of [`fltkhs`](https://hackage.haskell.org/package/fltkhs).
Unlike most of the other libraries mentioned here, `fltkhs` has very clear documentation for installation on all major platforms.
It is also easy to use.
Unfortunately, its looks are against it:
  though I regret to say it, I find FLTK to look extremely ugly by default.
However, themes are available through the [`fltkhs-themes`](https://hackage.haskell.org/package/fltkhs-themes) Haskell package.

Bindings to [Qt](https://www.qt.io/) also exist, via the [Qtah](http://khumba.net/projects/qtah) project.
Reportedly this works well, at least on Linux, and presumably on Mac OSX too.
However, it currently does not support Windows.
(In theory there is no reason why it should not work there;
  however, no-one has yet put in the time to fix this, though for almost a year now I [have been meaning to look into it myself](https://gitlab.com/khumba/qtah/-/issues/50).
I should get on to it…)

In addition to the major GUI libraries already listed, bindings to [wxWidgets](https://www.wxwidgets.org/) also exist,
  thanks to [wxHaskell](https://wiki.haskell.org/WxHaskell).
Unfortunately, wxHaskell has been long unmaintained (no commits for 4 years as of the time of writing), and I’ve found it nearly impossible to install, at least on Windows.
I have been informed that other people have had more success, but either way installation and usage seems to be extremely difficult with the current Haskell toolchain.

[`monomer`](https://github.com/fjvallarino/monomer) is a more recent Haskell GUI library.
It is unique in being a pure Haskell framework, though it uses bindings to [SDL](https://www.libsdl.org/index.php) underlyingly.
I have yet to use `monomer` myself, but it seems an excellent choice for GUIs which might otherwise be written with GTK+ or FLTK.

A somewhat different approach is provided by [dear-imgui.hs](https://github.com/haskell-game/dear-imgui.hs).
This library contains bindings to the [Dear ImGui](https://github.com/ocornut/imgui) framework.
As such, it inherits the usual advantages and disadvantages of immediate-mode GUIs
  (see e.g. <https://news.ycombinator.com/item?id=19745809>, <https://stackoverflow.com/questions/47444189> for relevant discussion).
Of interest is the fact that the immediate-mode paradigm seems on first glance to blend very well with Haskell’s usual style of functional programming,
  with the proviso that I have not tried ImGui myself.

Lower-level libraries are also available.
The [`sdl2`](https://hackage.haskell.org/package/sdl2) library gives Haskell bindings to [SDL](https://www.libsdl.org/index.php),
  while a somewhat higher-level interface is provided by [`gloss`](http://gloss.ouroborus.net/).
Both of these are general-purpose graphics libraries:
  they are intended for drawing arbitrary shapes on a window, rather than interactive GUIs.
However, it is certainly possible to implement a GUI framework on top of these (as indeed `monomer` does) if no other ready-made library suits.
This is the approach taken by many games.

Haskell also has mature bindings to the Win32 API, via the well-known [`Win32`](https://hackage.haskell.org/package/Win32) library.
These bindings allow the creation of native Windows GUIs, as described by numerous other tutorials (e.g. [theForger’s](http://www.winprog.org/tutorial/)).
However, this approach is extremely painful for all but the very simplest of message boxes.
There is no sense in using `Win32` for GUI applications, unless for some reason it is absolutely vital to create a native Windows GUI in Haskell.

It is also possible to write the underlying logic in Haskell, but use some other language for the GUI.
On Windows, I have had considerable success in linking a Haskell program to a GUI written in C++ using Qt;
  an outline of how this works is provided in the [Brassica architectural overview](https://github.com/bradrn/brassica/blob/73f55b6707289944ae20a15cde135294d4f7c4b6/ARCHITECTURE.md).
However, I have also heard that it is extremely difficult to get this approach working on Linux.
The conversation starting at <http://verduria.org/viewtopic.php?p=51303#p51303> may be of interest for the latter case.

A similar alternative is to convert a web-based GUI to a desktop application using [Electron](https://www.electronjs.org/).
This can easily be done using most, if not all of the web-based frameworks listed below.
Electron has certain advantages, most notably the ease of cross-platform porting.
As with other web-based approaches, it also allows great freedom in customising the look and feel of the resulting GUI, and provides access to the large Node.js ecosystem.
However, I personally prefer to use one of the dedicated cross-platform GUI libraries already mentioned.

## Web GUI libraries

In addition to the desktop-based GUI libraries already listed, Haskell has several libraries for creating web-based GUIs.
Perhaps the easiest of these to install and use is [`threepenny-gui`](http://wiki.haskell.org/Threepenny-gui).
This library creates native executables which run a small webserver, allowing a GUI to be displayed using a web browser.
`threepenny-gui` also includes support for FRP, which can be very helpful for certain usecases.
I have had success in using `threepenny-gui` for numerous different projects —
  often I use it to make an initial prototype GUI, which I can then rewrite using another library.
As briefly mentioned above, it is also easily possible to use Electron to create desktop applications using `threepenny-gui`.
A [guide to writing Electon apps with `threepenny-gui`](https://github.com/HeinrichApfelmus/threepenny-gui/blob/master/doc/electron.md) is available online.

Haskell can also be compiled to JavaScript using [GHCJS](https://github.com/ghcjs/ghcjs).
This is often desirable for applications which benefit from being easily accessible online.
Unfortunately, despite this (quite significant) advantage, GHCJS itself is somewhat painful to use.
Most prominently, GHCJS is difficult to install and use:
  in fact it is practically impossible to use GHCJS without the [Nix](https://nixos.org/) package manager.
(This is especially painful on Windows, though I find WSL works very well.)
It also uses a significant amount of memory, and the applications it outputs can be much slower than those produced by GHC.
However, GHCJS is still extremely useful in certain situations.

Several libraries are available for creating websites with GHCJS.
Lowest-level are [`ghcjs-dom`](https://github.com/ghcjs/ghcjs-dom), [`jsaddle`](https://github.com/ghcjs/jsaddle) and [`ghcjs-base`](https://github.com/ghcjs/ghcjs-base),
  which provide the basic interface for interacting with JavaScript and HTML from Haskell.
(Some examples of their usage may be found at <https://github.com/ghcjs/ghcjs-examples>.)
Notably, applications and frameworks which use `jsaddle` are compiler-agnostic:
  on GHCJS they produce a website, whereas on GHC they produce a small webserver as with `threepenny-gui`.

However, it is almost always easier to use a higher-level framework for GHCJS.
By far the most prominent libraries in this space are [Miso](https://github.com/dmjio/miso/blob/master/README.md) and [Reflex](https://reflex-frp.org/).
As both libraries are `jsaddle`-based, they may be used with both GHCJS and GHC —
  the latter mostly for development purposes, though.

Miso is based on virtual DOM diffing, allowing applications to be written with an Elm-like architecture.
Miso [has numerous advantages](https://www.reddit.com/r/haskell/comments/vsnrp5/comment/if9xxiu):
  most prominently, it is extremely fast and well-tested.
Though I have not used Miso myself, it also seems much easier to understand and use than Reflex does.
However, Miso is somewhat more difficult than Reflex to install and use.

By contrast to Miso, the Reflex library is based on FRP.
(In fact, it is actually a fully general FRP library, but in practice it is almost always used with GHCJS via [`reflex-dom`](https://hackage.haskell.org/package/reflex-dom).)
I have found Reflex somewhat inflexible and difficult to use, especially for very highly dynamic GUIs,
  as outlined in e.g. [reflex-frp/reflex#461](https://github.com/reflex-frp/reflex/issues/461).
Miso seems like it might cope better with such problems, though I have not yet used it myself.
However, Reflex is much easier to install than Miso, thanks to the existence of projects such as
  [Reflex Platform](https://github.com/reflex-frp/reflex-platform) and [Obelisk](https://github.com/obsidiansystems/obelisk).
  
## Other libraries

In addition to the libraries already mentioned, a number of niche or less actively developed GUI libraries are also available:

- [Fudgets](http://www.altocumulus.org/Fudgets/) is by far the oldest GUI library for Haskell, having been developed continuously since 1991 (before Haskell98!).
  It allows the creation of GUIs for the X window system using a unique GUI paradigm with some resemblance to FRP.
- [webviewhs](https://github.com/lettier/webviewhs) contains bindings from Haskell to the [webview](https://github.com/webview/webview) library.
  However, it has seen no commits for three years as of the time of writing.
- [Momentu](https://github.com/lamdu/momentu) is a framework for animated desktop GUIs with responsive layout.
  It is currently actively used to create the interface for [Lamdu](http://www.lamdu.org/).
  However, its documentation is extremely poor.
- [Concur](https://github.com/ajnsit/concur) is a framework for the creation of web-based GUIs.
  Its GUI model is advertised as combining FRP with the Elm architecture.
  However, Concur is [no longer actively maintained](https://www.reddit.com/r/haskell/comments/vsnrp5/comment/if4anre/) and is not recommended for any new GUIs.
- [Shpadoinkle](https://shpadoinkle.org/) is another web-based GUI library, with similarities to Concur.
  Its maintenance status is unclear: no changes to the code have been made in over a year, but there is still some activity on the repository.
