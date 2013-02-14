This is a Racket library that allows one to draw a text along the curve plus one
simple GUI app that serves as a demonstration.

A picture may tell you more about what does it really do, so here it is:

![](https://github.com/piotrklibert/bezier/raw/master/priv/screenshot.png)


This is a module dependency graph - I thought that I include it for anyone
who'd like to read through the code to provide a hint where to start reading:

![](https://github.com/piotrklibert/bezier/raw/master/priv/modules.png)

Development history
===================

It probably is not very useful, not even to me, but I wrote this code to learn
more about Racket, to explore the tools it gives me as a programmer. I did my
best and I spent quite a lot of time on several rewrites of the code, because I
wanted to make it as idiomatic as I possibly could.

Over the past year I was learning about it's features and now I decided to
learn-by-doing about how these features fit together. I was a bit afraid that
all these nifty features would feel awkward when actually used.

Let me tell you: they were not awkward at all, every single feature I used was
very comfortable, clean and elegant. I still only used maybe a third of what Racket
offers, but - and it's incredible - these features I used were more than enough
to work through this simple library and I suppose they would scale to any
mid-sized project easily. For even bigger things there are tools like units and
collections; for more advanced things there are tools like macros and for
performance critical things there are tools like Typed Racket and futures. So I
think that Racket has potential to become a very successful langueage.

Evaluation of Racket usefulness
===============================

Ok, now to the details. Features I used with a bit of explanation on why I used
them and how it felt, i no particular order:

* Contracts. Now this is something that I already know I will miss dearly when
  programming in Python and JavaScript from now on. This is not about static
  typing (that much) - in practice it allows you to write the bodies of
  functions in a way that directly reflects you domain, without worrying about
  inappropriate arguments. Error handling, for the most part, is shifted outside
  of the function and it's great, you can focus on doing what you want with the
  input you wanted.

  Also, it's surprising how informative a contract can be. Given the fact that
  you can (and should) declare your own contracts it frequently is enough to
  read the contracts on thing that a module exports to know what the module is
  supposed to do and even how to make it do this. 

  One thing of note is that when using contracts with provide form you do not
  get contract checking when functions are called inside this module. For this
  situation there is define/contract form which I used a few times.

* Structs. Not really much to say here... They are so powerful in Racket yet I
  didn't use even a half of functionality they provide. You can make a structure
  to behave like a function, you can make it implement any number of generic
  interfaces and use it in places that require things with certain properties
  (for example you could write your own file-like object or your own
  collection), you can even inherit structs... And yet I felt no need for any of
  this functionality. Ok, in one place I made "point-and-angle" struct inherit
  from "point" struct instead of giving it a point field, because accessors were
  shorter.

  The thing is that all this possibilities fall back to sane defaults when you
  don't need them, giving you everything you could want should you need it.

* Iteration. Racket is supposed to be a functional language and indeed, not
  counting the bit of classical OOP in GUI handling there is not one destructive
  update. Racket is a Scheme, so tail-calls are optimized and tail-recursion is
  always an option, but... Not always the best option. There is a whole family
  of for macros, which are insanely overloaded with functionality and come in
  very different shapes and sizes. Three 'named let's I wrote initially I later
  rewrote as for/fold and for/vector, which considerably lowered the complexity
  and line count. Also, these macros are meant to work with anything that
  supports iteration protocol (much like Python's for) and they don't care
  whether they get a list, a string or a vector to iterate over. It's really
  handy.

* OOP. That's shocking, right? No, some things are really best modeled with
  objects, classes and their hierarchies. Once again, I used just a tiny
  fraction of what Racket's object system provides - I just needed to subclass a
  canvas class and override its 'on-paint' method. It was a breeze, just like
  with structs, OO features fall back to sane defaults and it's really easy to
  write a working code. It's somewhat more difficult to create pretty code here
  and integration with contracts is lacking - either I failed to find or there
  is no define/public/contract form. Other than that Racket's object system is
  among the best I worked with and in some areas significantly better than
  others (augment? yeah, you can - just barely - pull this off in C++ with
  templates, but the bloat is incredible then).

* Macros. I didn't use any. Really, the few that I wrote are there because they
  are effectively forcing inlining and are not really needed if I use something
  like define-inline. However, I refuse to use things I don't understand and so
  define-inline has to wait for it's turn. The thing here is that I stared hard
  and couldn't think of any use for macros. I since thought of some potential
  uses for macros and will be experimenting with them, but the base language was
  (more than) enough to complete this project.

* Everything else. There is so much I didn't use or used for a while and dropped
  because of time constraints. Unit tests, in-source documentation and Typed 
  Racket come to mind. There is really no shortage of tools, concepts and
  features in racket-land and so far I haven't really found anything below 'very
  good' with average bordering 'awesome'... 

There is much more to be written about Racket in general and about my adventure
with it specifically and I will write more when I get the time. For the time
being I'm just happy I managed to write something completely useless in an
esotheric language that no one ever heard about and that the code I wrote is not
an abomination I see daily at work. 

That's all :)
