# hs-ircv3

[Attoparsec](https://hackage.haskell.org/package/attoparsec) parser for [IRCv3](https://ircv3.net).

## Example usage

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Attoparsec (parseOnly)
import Data.IRCv3 (message, Message)

main :: IO ()
main = do
    let msg = ":tmi.twitch.tv 001 justinfan123 :Welcome, GLHF!\r\n"
    print $ parseOnly message msg
```

```
> Message {tags = Nothing, source = Just "tmi.twitch.tv", command = "001", parameters = (["justinfan123"],Just "Welcome, GLHF!")}
```

## References

- https://datatracker.ietf.org/doc/html/rfc1459
- https://datatracker.ietf.org/doc/html/rfc2812
- https://datatracker.ietf.org/doc/html/rfc3986
- https://ircv3.net/irc/
