---
title: LED Animations II: Monads
author: uwap
tags: Haskell, Concepts, Monads
---

Last week I did a blog post about LED Animations and how
functions get us composability. Though last time we had some
open problems that our concept couldn't solve.

* We can't create random frames since the type of AnimationStep is `Frame -> Frame`{.haskell}
* We can't create animations like a sinus brightness fade because once everything is black we don't have
the information about the previous color.

As a conclusion to the last post we need to make animations monadic.
For simplicity we will use IO as our only monad.

First let's change some defintions.
To not further confuse Animation and AnimationStep, we will say that
Animation is a function, so basically what AnimationStep was in the last post.
We don't need to store animations as lists anymore. Why? I will explain it later.

Since we want to make animations monadic, let's go!

```{.haskell .numberLines}
type Animation = Frame -> IO Frame
```

Sadly this means we can't compose our animations by `(.)`{.haskell} anymore.
Though, there is a compose function that will even work on this type.

<!--more-->

```{.haskell .numberLines}
(>=>) :: (a -> m b) -> (b -> m c) -> a -> m c
```

Given two animations a and b the type of `a >=> b`{.haskell} is Animation.

That seems to work. (>=>) is the compose function for monadic functions.
And that's just it. Composability works :)

All functions we had for our pure definition (`Frame -> Frame`{.haskell}) can be
easily converted into the monadic version.

Last time we had the definition of reduceBrightness:

```{.haskell .numberLines}
reduceBrightness :: Frame -> Frame
reduceBrightness frame = (*) (200, 200, 200) <$> frame
```

Let's make it monadic!

```{.haskell .numberLines}
reduceBrightness :: Animation -- That is: Frame -> IO Frame
reduceBrightness frame = return ((*) (200, 200, 200) <$> frame)
```

A simple return to return the new frame into our IO monad.

The first problem was that the random function won't work on type `Frame -> Frame`{.haskell}.

```{.haskell .numberLines}
fillRandom :: Frame -> IO Frame
fillRandom _ = replicateM numPixels randomColor
  where
    randomColor = (,,) <$> randomIO <*> randomIO <*> randomIO
```

There we go!

## The big problem: The brightnessSin function

We have one big of a problem. Imagine a function changing the brightness of
a current frame in form of a sine. Right now we can't make such a function because of two
difficulties:

* The brightness depends on how often we called the function. We have to find some point
where we don't go darker but start to go brighter again
* Once we reach the point where everything is black how are we supposed to make it lighter again?

We will see that such function can't be made with our current system but we can do a similar function.

The colorSin animation is a simplified version. It takes a color and fills everything with that color
and changes the brightness in form of a sine.

```{.haskell .numberLines}
colorSin :: Color -> Animation
colorSin color _ = ...?
```

We have a problem here. As mentioned above we need the ability to count.

### Counting!

We want a way to get information about how many frames we had before the current frame.
We are going to call this number i. That means on the first frame i = 0. On the second one i = 1.

Obviously i is a variable and not a constant. That is a bad thing.
Luckily we have IORef and we are working within IO anyways.

```{.haskell .numberLines}
type Counter = IORef Integer

colorSin :: Counter -> Color -> Animation
colorSin counter color _ = do
  i <- readIORef counter
  ...?
```

That way we have a counter that can increase every time we render a frame.
Now we just need a function sinFactor that returns a number between 0 and 1 depending on the Integer
and a function setBrightness that takes a Double and a Color and returns a new Color.

```{.haskell .numberLines}
sinFactor :: Integer -> Double

setBrightness :: Double -> Color -> Color
```

and now we can finally create colorSin.

```{.haskell .numberLines}
colorSin :: Counter -> Color -> Animation
colorSin counter color _ = do
  i <- readIORef counter
  let frame = replicate numPixels color
  return (setBrightness (sinFactor i) <$> frame)
```

That way we have created a similar function to brightnessSin.
The good news: We could even easily create frameSin.

```{.haskell .numberLines}
frameSin :: Counter -> Frame -> Animation
```

The better news: There is also a way to implement brightnessSin.

## BrightnessSin

To show the difference between colorSin, frameSin and brightnessSin
here are the type signatures:

```{.haskell .numberLines}
colorSin      :: Counter -> Color -> Animation
frameSin      :: Counter -> Frame -> Animation
brightnessSin :: Counter -> Animation
```

The problem with brightnessSin is that once we our sinus changed the brightness
to black we have no way of restoring the initial frame.

At this point I was kind of distracted because there are workaround to create that function
but they are all not that beautiful. Luckily there are people who are more creative than I am
and so I can even present you a solution for this problem. Many thanks to my boyfriend [\@RanlvorPub](http://twitter.com/RanlvorPub)
for finding an awesome way to solve this issue.

### Changing the definition of Color

Right now we get the brightness through our color implicitely.
The color (0,0,0) is considered black. But if we add brightness as a seperate
value to our color, then for any r,g,b (0,r,g,b) is considered black.

```{.haskell .numberLines}
type Color = (Double, Word8, Word8, Word8)
```

That means our new color consists out of a brightness and r,g and b values.
With that workaround we can change the brightness of the color until it is black
and even then we are able to restore our initial color.

```{.haskell .numberLines}
brightnessSin :: Counter -> Animation
brightnessSin counter frame = do
  i <- readIORef counter
  return (setBrightness (sinFactor i) <$> frame)
```

That still bothered me a bit. What if a previous animation changed the brightness already?
We are completely overriding the brightness value here.
And why is `(brightnessSin >=> brightnessSin) == brightnessSin`{.haskell}? That makes
absolutely no sense.

And even here we can find a simple workaround.
Instead of setting the brightness we just increment it by the difference between `sinFactor i`{.haskell}
and `sinFactor (i-1)`{.haskell}. That way `brightnessSin >=> brightnessSin`{.haskell} won't
change the brightness in form of $sin(i)$ but in form of $sin²(i) = sin(sin(i))$ just as it was
supposed to be.

## Ana?

Last time we had an anamorphism `ana :: (a -> a) -> a -> [a]`{.haskell}.
This time we can't create such anamorphism which has multiple reasons.
One of them being our counter. I want a "global counter".
So every function which asks for a counter will get the same counter.

Instead of ana we will just create a runAnimation function.

```{.haskell .numberLines}
runAnimation :: Counter -> Animation -> Frame -> IO ()
runAnimation counter f startFrame = run startFrame
  where
    run frame = do
      renderFrame frame
      modifyIORef counter (+1)
      nextFrame <- f frame
      run nextFrame
```

And that's it. I hope you like this second blog post and I would be glad to get some feedback.
Once my LED Strips are done I will show you a video of the LEDs in action.
