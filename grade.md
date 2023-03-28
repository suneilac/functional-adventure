# Overall Grade

96%

## Comments

Nice work this quarter, you've consistently turned in excellent work.

# Grade Components

## User Feel

10/10

The game plays smoothly. It's hard! I was almost sure the second level was
impossible until I realized you could go `west` a second time from the entrance.
I liked that I needed to visually memorize my steps with the lantern before
dropping it and making a run to the `yard` with the `jug`.

## Bug-Free Performance

10/10

I wasn't able to cause any runtime bugs nor produce any incoherent game states.

## Required Minimal Features

10/10

All required minimal features are present.

## New Features

9/10

Nice job on the new features, I really enjoyed the `latern` mechanic. While
other folks added some kind of "darkness" feature, you were the only one to have
an item in your inventory to allow viewing rooms in darkness and set the weight
of the item such that you couldn't finish the game while carrying it. I thought
this was a very clever design.

I knocked off one point here because you didn't customize the content at all.

## Documentation

10/10

Your `README` is easy to understand and provided all required information.

## Code Style

9/10

Your code is easy to read and understand. There are a few areas where you could
simplify things a bit.

### Main Function

Your `main` function ran one `execStateT` for `opening` and a second
`evalStateT` for the `repl` loop. However, we can combine these two effect-ful
values with `>>`! Here's your code:


```haskell
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    s <- execStateT opening initialState
    evalStateT (forever repl) s
```

And here's equivalent code:

```haskell
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  evalStateT (opening >> forever repl) initialState
```

