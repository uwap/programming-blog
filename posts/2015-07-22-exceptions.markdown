---
title: Exceptions are exceptionally painful
author: uwap
tags: Haskell, Concepts, Exceptions
---

In todays post I want to show you my experience about
exception handling. As in previous posts I want to focus on Haskell
though this might apply for other languages too.
With this post I want to open a discussion on exceptions so I want to hear
*your opinion* about it.

## Errors v.s. Exceptions

First of all I want to talk about the differences between errors and exceptions to
make my point clear. The distinction between those is not always clear but for
simplicity I want to give an explicit definition focused on an entry in the
[Haskell-Wiki \[1\]][1].

If we think about the word exception it has a similar meaning to "a special case".
This is exactly the definition I want to use here. There is some exceptional case
of input that our program has to handle.

An error may be something totally unexpected as defect hardware or simply
logic errors in the code, basically everything that isn't an exception.

This blog post is about the exceptions, not the errors.

<!--more-->

## My intention in this blog post

Currently I am working on a network application. The thing with network application is
that it is important to prevent unexpected crashes to keep to connection stable.
Later I will show that exceptions are no reason for a server to crash.
However, some functions are implemented to crash when it gets some unexpected input.
For network applications this is the worst case.

Later I will show that exception handling is important and that in
some cases the programmer shall be forced to catch all exceptions
and in other cases exceptions can be ignored completely.

## Handling exceptions

As mentioned above, exceptions can be seen as an exceptional case.
What shall a program do with an exceptional case? Of course, it shall handle it.

A very common exception are non-total functions. For every non-total function f
there exists an x such that f(x) is not defined. This leads to the following question:
What shall we do in that case?

There are a lot of possibilities to fix it.

### Throwing an error

In Haskell `head`{.haskell} is defined like this:

```{.haskell .numberLines}
head :: [a] -> a
head []    = error "head: Empty List"
head (x:_) = x
```

This will make the whole program crash when calling `head []`{.haskell}.
It somehow turns an exception into an error. Why is this?

This is because head is not defined on empty lists. One solution is only
defining head on empty lists which I will mention later on. Instead of
changing the domain we could also change the codomain.

### Maybe!

```{.haskell .numberLines}
headMaybe :: [a] -> Maybe a
headMaybe []    = Nothing
headMaybe (x:_) = Just x
```

With Maybe we can make our codomain fit. The problem is that head is
so primitive that pattern matching against the maybe is probably more
work than pattern matching against the list itself. We just made a useless function.

And this is where we begin to see the other side: Simplicity.

The sad truth is that incorrect functions are simple. It is so easy to simply use
list and I think it is totally fine for "just quickly writing something", learning
or writing a prototype though it shouldn't appear in real world, which means
that we have to make a seperation between safe and unsafe function and in my opinion
head is an unsafe function.

### Monads!

Since modifying the codomain to Maybe we have a correct function
which doesn't actually do exception handling. Instead it kind of
returns the exception so the caller is due to do the exception handling.
This has a simply reason: There are applications where it is good enough to just
crash and there are applications where a crash shall be prevented; There are
applications where it is good enough to just print an error message to the stderr
and there are applications where a whole stack trace shall be printed in a log file.

There are various cases of handling exceptions and many of them are so different and
though there is an abstraction for all of them.

In Haskell this abstraction exists in [Control.Monad.Catch \[2\]][2] and it is called MonadThrow.
This is basically a type class providing a throwM function:

```{.haskell .numberLines}
class Monad m => MonadThrow m where
  throwM :: Exception e => e -> m a
```

and now we have a nice way of throwing our exceptions: throwM.
With throwM the caller can decide how he wants to handle the exception.
For example, if the caller wants a Maybe a back, then he gets his Maybe.
This is because there is a type instance

```{.haskell .numberLines}
instance Maybe a where
  throwM = Nothing
```

Now think of head. We could implement it in a way that it uses MonadThrow.

```{.haskell .numberLines}
head' :: MonadThrow m => [a] -> m a
head' []    = throwM $ PatternMatchFail "head only accepts non empty lists"
head' (x:_) = return x
```

Now if we want to our program to throw an IO Error if head was given the wrong parameter
then we can easily create an instance for MonadThrow IO.

This gives the caller enough ability to choose a method of exception handling he likes.

### Dependent types

Even better in my opinion would be changing the domain of head to just non-empty lists.
Dependent types allow that. Below is some idris implementation for a safe head function:

```{.idris .numberLines}
head : (l : List a) -> {auto ok : NonEmpty l} -> a
head (x :: _) = x
```

That forces you to prove your list has an element before tossing it into head.
It allows you to be totally free when it comes to exception handling.

## Conclusion (That is actually the important part)

I think that it is important to allow better exception handling and not just randomly
throw errors out. For some applications it is totally fine if they crash, for others
it is so important that they don't. In my case, writing a networking application
it is important that no crash will occure and therefore I kind of want to ban all those
unsafe functions out that don't allow proper exception handling.

Even further: I want the compiler to force me catch all exception because I am sure I will
always forget something. This is not so good for just some quick hack or anything because
if you write just simple small code it might bother you and you might become afraid of handling
all those exceptions that might not even appear for you.

Though, assuming that there are the two cases that either want all exceptions to be handled or
quick and dirty code it would mean deviding the libraries into two parts each bringing different
implementations of certain functions. Prelude.Unsafe could bring `head :: [a] -> a`{.haskell} and
Prelude.Safe could bring `head :: MonadThrow m => [a] -> m a`{.haskell}. That would though raise the
barrier for beginners.

What do you think about exception handling? How can we assure safety? Do you think there is a better way?
I'd be glad to hear your opinion. I'm open for discussion via [twitter](http://twitter.com/TheUwap) or
[email](mailto:me@uwap.name), too.

#### References

\[1\]: [Haskell Wiki - Error vs. Exception][1]    
\[2\]: [Hackage Docs - Control.Monad.Catch][2]

[1]: https://wiki.haskell.org/Error_vs._Exception
[2]: https://hackage.haskell.org/package/exceptions-0.8.0.2/docs/Control-Monad-Catch.html#t:MonadThrow
