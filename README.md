# Advent of Code 2024

Martin Fowler has 0 repos.

[Uncle Bob](https://github.com/unclebob/AOC24) has a repo for '24 where he did 14... so I'll do 14.

## Sketches

Github user jabbalaci made pretty sketches for some of the more interesting days.

I thought that this is a very neat idea!

So let's do some sketches!

### Goal

I picked specifically __Day 02 Part B__ for this purpose.

Solutions I saw were O(N * N) for one line of input. Aka, for my [big input](src/d02/punish-quadratic.txt) they take around 3 minutes.

3 minutes?! How about 3 secs, so...

We want to prove that __O(N)__ solution is __trivial__ and __hollywood__.

### Subgoals

__O(N)__ meaning it is still whooshy speedy on the big input too.

__Trivial__ meaning - in latin - `tri -> three` and `via -> way`. Aka there exist at least 3 different kinds of way of doing it.

__Hollywood__ meaning the idea must be linked to a movie or TV Show.

### The variants

| nickname | movie / show | idea | link to sketch |
| :------- | :---- | :--- | :--- |
| Tuco Salamanca | Breaking Bad | brute DFA / math hammering | [Raymond Cruz sketch](src/d02/d02-linear-tucosalamanca.pdf) |
| Maneater | No Hard Feelings | greedy | [Jennifer Lawrence sketch](src/d02/d02-linear-maneater.pdf) |
| Temporal Pincer | Tenet | prefix-suffix, array access | [Robert Pattinson sketch](src/d02/d02-linear-temporalpincer.pdf) |
| Temporal Pincer, eliminate KMI9 KMJ | Tenet | prefix-suffix, zip shift | [John David Washington sketch](src/d02/d02-linear-temporalpincer-eliminate-kmi9kmj.pdf) |

Welcome to my repo!

> And as always: Before checking out my code, don't forget the [check-out can](https://youtu.be/T5fXB1Dr1Tc)!

## Is Haskell on npm? Is it behind a load balancer? Is it following the Figma pixel-by-pixel? Does Haskell count into EBITDA?

No, no, no and no.

Some other questions / answers below...

__Does null exist?__

Yep:

```
null [1, 2, 3] == False
```

__Is there an undefined?__

Yep:

```
True || undefined == True
```

__Can I compose big computations from small ones?__

Yep, backwards:

```
bigprogram = ending . middle . beginning
```

__Can I define a function which does NOT return a number?__

Yep, for example this one:

```
f x y = x + y
```

__Can I define a function which DOES return a number?__

Yep, here's one:

```
f _ = 1
```

__Can I define a function which has ONE input and returns a number?__

Easy peasy lemon squeezy:

```
f (x, y) = x + y

f (1, 2) == 3
```

__Are there more?__

Yep, there are a lot of these... for example:

```
(f 1)   2 == (-1)
(`f` 1) 2 == 1
-- and f being
f x y = x - y
```

__Can you show a refactor?__

Of course:

```
f x y = x + y
-- move out +
f x y = (+) x y
-- y is self-explanatory, so it can be inferred
f x = (+) x
-- x is self-explanatory, so it can be inferred
f = (+)
-- f is bloat, only a new name for (+)
-- can be removed
```

Refactoring is complete.

Necessary code left after refactoring:

```

```

__Can you show an infinite loop?__

Here you go:

```
[0..]
```

__Are there classes, instances?__

Yes. Yes there are:

```
class Foo v where
  foo :: v -> v -> v

data Bar = Bar Int

instance Foo Bar where
  foo (Bar x) (Bar y) = x + y
```

There are even constructors:

```
data Cat = Cat { getName :: String } | Santa { getPresentCount :: Int }
```

__How can I define a tree?__

```
data Tree a = Node a [Tree a]
```

__How to implement traversal?__

No implementation necessary. Haskell can derive it on its own.

__What is a monad?__

[A box](https://youtu.be/1giVzxyoclE?t=135).
