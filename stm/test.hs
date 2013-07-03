import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

arrayRed::Int-> Int -> STM ()
arrayRed t th = do
    --allocate t arrays
    ; arr <- atomically $ newTVar (take t [1 ..])
    --parallel divide of 1 over
    ; forkIO $ th `timesDo` (appV (\x-> 1/x ) arr)
    milliSleep 800
    ; after <- atomically . readTVar arr
    --reduction(atomic)
  where timesDo = replicateM_
        milliSleep = theadDelay . (*) 1000
appV fn x = atomically $ readTVar x >>= writeTVar x . fn
