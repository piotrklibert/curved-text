# curved-text - simple library for drawing text along the curve

## What?

A simple library for drawing text along the cubic Bezier curves. It's probably
easier to show than describe, so here's a screenshot of DrRacket REPL showing
the library in action:

![](https://github.com/piotrklibert/bezier/raw/master/priv/demo-inline-screenshot.png)

In addition to the library itself there's a little GUI demo app, which displays
a curved text and allows manipulating control points by dragging them with the
mouse:

![](https://github.com/piotrklibert/bezier/raw/master/priv/demo-screenshot.png)

## How?

You need to have Racket installed and `raco` in your `PATH`. You can then
install and run it with the following commands:

    raco pkg install curved-text
    racket -l curved-text
