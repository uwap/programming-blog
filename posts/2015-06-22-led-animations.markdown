---
title: LED Animations I: Functions for composability - Co-Algebraic modelling
author: uwap
tags: Haskell, Concepts, Composability, F-Algebras
---

As I moved into my current flat there was one thing I wanted to do:  
I wanted my lights to be LEDs that can change their colors and intensity
all individual so I can program Animations for it.

Today I want to present a dynamic way of creating and composing animations.
As a lot of projects I've seen have webinterfaces where you can select a
hard-coded animation/mode I wanted to have a webinterface that goes beyond that.
A webinterface in which you could create your own animations!

Imagine an animation called 'sinBrightness' that takes a color and renders
the color over all pixels given a certain brightness. This brightness modifies
over time by a sine.

Now instead imagine sinBrightness only modifying the brightness of every pixel.
You end up with much higher composability.

<!--more-->

# Some Definitions

While my LED Strip consists out of pixels I was looking for an apropriate data type
to represent these. The data type I created is called a Frame. The LED Strip displays
exactly one frame at a time. A frame is an ordered list of colors. A color is tripel of
unsigned bytes.

```{.haskell .numberLines}
type Color = (Word8, Word8, Word8)
type Frame = [Color]
```

Since this is haskell, sadly I can't type check the frame against my exact number of pixels on the LED Strip.
Anyways, now defining what the LED Strip shall display is really easy. If I want all pixels blue I will define
a frame 'blueFrame':

```{.haskell .numberLines}
blueFrame :: Frame
blueFrame = replicate numPixels (0,0,122)
```

If I want all pixels of my frame to be random colored, I just define a frame 'randomFrame':

```{.haskell .numberLines}
randomFrame :: IO Frame
randomFrame = replicateM numPixels randomColor
  where
    randomColor = (,,) <$> randomIO <*> randomIO <*> randomIO
```

And what if I want animations?
I can define a list of frames!

```{.haskell .numberLines}
type Animation = [Frame]
```

An Animation is a lazy evaluated list of frames.
Let 'rgbAnim' the animation that displays everything red, then green, then blue and then terminates.
We can now define 'rgbAnim':

```{.haskell .numberLines}
rgbAnim :: Animation
rgbAnim = [red, green, blue]
  where
    red   = replicate numPixels (122,0,0)
    green = replicate numPixels (0,122,0)
    blue  = replicate numPixels (0,0,122)
```

# Limitations of lazy evaluated lists

While lazy evaluated lists have a lot of abilities like handling infinite animations they quickly run into
limitations. For example, remember the definition of 'randomFrame' above?
What if I want to actually change the random every tick?
That is, I want an animation that constantly changes all colors of the frame randomly.
Because of the given type `[Frame]`{.haskell} this is not possible.
We could change the type of Animation to `[IO Frame]`{.haskell} instead to make things like these possible
but we will notice that we still run into limitations, especially when we get to composability.

## Functions! All hail functions!

Functions are our solution here. First of all let's get into a bit of theory.

### Anamorphisms

A list can be created by "destructing" our animation into several steps.
That is a function `a -> a`{.haskell} and an anamorphism.
Our anamorphism looks like this:
```{.haskell .numberLines}
ana :: (a -> a) -> a -> [a]
ana f a = f a : ana f (f a)
```
That functions applies our function over and over again and appends each result to a list.

### AnimationStep

An animation step is a function that takes a frame and creates a new frame.

```{.haskell .numberLines}
type AnimationStep = Frame -> Frame
```

Now all our problems are solved! We get composability for free!

Imagine an AnimationStep Function that does the following:

* Take a specific color (for example blue)
* Set the color of the first and the last pixel to black
* For every color from the first on increase the brightness until you reach the most centered pixel
* From the center on, reduce the brightness until you reach the last pixel

This function should have the brightest point in the center and the darkest on the edges.
For our example let's create a function 'centerBlue' that does exactly this with the blue color.

```{.haskell .numberLines}
centerBlue :: AnimationStep
centerBlue _ = center (0,0,122) numPixels
```
where 'center color pixels' does exactly what described above.

Let 'reduceBrightness' be an AnimationStep that constantly reduces the brightness of a given frame:
```{.haskell .numberLines}
reduceBrightness :: AnimationStep
reduceBrightness frame = (*) (200, 200, 200) <$> frame
```
The animation that reduces the brightness of 'centerBlue' is now given through $reduceBrightness \circ centerBlue$

### Ana for AnimationStep

We can now get a lazy evaluated list back out of these functions.
That means: We don't have to give up an easy data type to store our animations in
plus we get full composability through functions.
The Animation of an AnimationStep Function `f`{.haskell} is simply `ana f startFrame`{.haskell},
where Animation still follows the definition from above: A list of frames.

## Monads?

With these changes we got composability for our animations. That still doesn't solve the problem
of our randomAnimation. We can't no longer hide the truth: We need monadic functions!

I already have a working solution for monadic functions that tries to follow the conecpt explained above at best
but it has to wait for another blog post.
That means: In the next part I'll explain how a monadic version of AnimationStep could work and what
other limitations we have with lazy evaluated lists (for example 'sinBrightness').

I hope you got a good view on how functions can help us bring composability in our programs
and how we can still keep our old data types thanks to co-algebras (anamorphisms). 
