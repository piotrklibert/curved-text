# curved-text - simple library for drawing things along the curve

## What?

A simple library for drawing text (but it can be used for drawing other things
too) along the cubic Bezier curves. It's probably easier to show than describe,
so here's a screenshot of DrRacket REPL showing the library in action:

![](https://github.com/piotrklibert/bezier/raw/master/priv/demo-inline-screenshot.png)

In addition to the library itself there's a little GUI demo app, which displays
a curved text and allows manipulating control points by dragging them with the
mouse:

![](https://github.com/piotrklibert/bezier/raw/master/priv/demo-screenshot.png)

I'm refactoring the code right now to make it easier to draw objects other than
text along the curve. Even if you don't need to draw such things reading the
code may still be worthwhile if you want to learn about how Bezier curves work.
The code is relatively straightforward and self-contained, it only depends on
Racket for now.

## How?

You need to have Racket installed and `raco` in your `PATH`. You can then
install and run it with the following commands:

    raco pkg install curved-text
    racket -l curved-text

## Why?

A few reasons:

* I wanted to learn about Bezier curves and how they work
* I wanted to test Racket usefulness as a language for projects larger than a
  100 LOC (it's around 1kloc currently)
* when I started writing the code I realized that there's no easy way for
  drawing curved text in Racket

In general there is no problem if you want to draw the curve itself, but APIs
for drawing things along the curve are missing. The only way of doing this is to
compute the curve and its derivative yourself, which is what I did.

I started the project 4 years ago and spent quite a lot of time optimizing the
code so that the demo runs smoothly. Initial versions were very slow and I ended
up resorting to optimized flonum functions, `begin-encourage-inline` and even
defining macros instead of function in some cases. In the end I managed to make
the code fast enough to draw the curve and text along it without skipping frames
or boiling the CPU. It was written with Racket < 6.0, I think, or some early 6.x
release. I left un-optimized code to use it in unit tests of the optimized
versions.

Imagine my surprise when I revisited the code recently (when Racket 6.8 was
released) and saw that both versions perform exactly the same. It looks like
Racket got much, much better in optimizing the code in the meantime, which is
great!

Another great thing that happened while I was away from Racket was migrating off
of PLaneT and to new `raco pkg` system. When I realized that the code was now
fast enough to be useful, I decided to make it into a package and publish it.

I plan to continue working on the refactor, both to make the code more general
and to make it into a literate description of how Bezier curves work.

Originally I included a description of Racket features I used when writing the
library and I plan to add it back once I finish refactor. For now I can only say
that Racket became even more awesome than it was four years ago and that working
with it is a great pleasure :)

## TODO

A lot of things, but the most important are these:

* rewrite math parts in Typed Racket
* make fonts customizable
* allow for drawing arbitrary shapes along the curve
* add more unit tests and benchmarks
* add some documentation and comments to the code
* possibly improve the demo program to allow for more than one curve
