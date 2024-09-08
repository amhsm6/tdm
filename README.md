# Telegram Database Monad

A simple monad wrapping [haskell-tdlib](https://github.com/mejgun/haskell-tdlib). \
Allows for quick unwrapping Maybes and (if that seems useful) manages a queue of tdlib messages to potentially return back message that was supposed to be processed by another handler.

## API

TDM module is supposed to replace both TD.Lib and TD.GeneralResult.

```haskell
start :: TDM a -> IO () -- entry point, creates new client

auth :: TDM () -- perform authorization based on env API_ID, API_HASH and PHONE_NUMBER

get :: TDM GeneralResult -- receive a message

put :: GeneralResult -> TDM () -- put message back in the queue, probably useless

send :: ToJSON a => a -> TDM () -- send a message
```

Check out src/Monad.hs for other features.
