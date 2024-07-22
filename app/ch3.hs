-- repl.it link: https://replit.com/join/viwsnnysqx-mdkennedy03
import Control.Concurrent
import Control.Concurrent.STM
import Test.HUnit

-- Define a type for a shared counter
type Counter = TVar Int

-- Function to increment the counter
incrementCounter :: Counter -> STM ()
incrementCounter counter = do
  value <- readTVar counter
  writeTVar counter (value + 1)

-- Function to decrement the counter, with a blocking mechanism if the counter is zero
-- Concept: The retry function allows a transaction to block and wait for certain conditions to become true.
-- If value is not greater than 0, the transaction will block and wait for the counter to be incremented.
decrementCounter :: Counter -> STM ()
decrementCounter counter = do
  value <- readTVar counter
  if value > 0
    then writeTVar counter (value - 1)
    else retry -- Block if the counter is zero

-- Function to try decrementing one counter or another, using orElse
-- Concept: The orElse function allows the composition of alternative transactions, where the second transaction is attempted if the first retries.
-- This function will attempt to decrement counter1 and, if it retries, will then attempt to decrement counter2.
tryDecrementEither :: Counter -> Counter -> STM ()
tryDecrementEither counter1 counter2 =
  decrementCounter counter1 `orElse` decrementCounter counter2

-- Function to create a new counter with an initial value
-- Concept: TVars are mutable variables that can be read and written within STM transactions
-- Here, Counter is defined as a TVar Int, and newCounter creates a new TVar initialized with the given value.
newCounter :: Int -> IO Counter
newCounter initial = atomically $ newTVar initial

-- Main function to demonstrate the composable transactions
-- Concept: Transactions ensure that a series of memory operations occur atomically, i.e., they either complete entirely or have no effect at all.
-- The atomic function is used to ensure that operations on TVars are performed atomically.
-- Unit tests
main :: IO ()
main = do
  -- Unit test for incrementCounter
  testIncrementCounter <- runTestTT $ TestCase $ do
    counter <- newCounter 0
    atomically $ incrementCounter counter
    finalValue <- atomically $ readTVar counter
    assertEqual "Incrementing counter" 1 finalValue

  -- Unit test for decrementCounter
  testDecrementCounter <- runTestTT $ TestCase $ do
    counter <- newCounter 1
    atomically $ decrementCounter counter
    finalValue <- atomically $ readTVar counter
    assertEqual "Decrementing counter" 0 finalValue

  -- Unit test for tryDecrementEither
  testTryDecrementEither <- runTestTT $ TestCase $ do
    counter1 <- newCounter 0
    counter2 <- newCounter 1
    atomically $ tryDecrementEither counter1 counter2
    finalValue1 <- atomically $ readTVar counter1
    finalValue2 <- atomically $ readTVar counter2
    assertEqual "Try decrementing either counter1 or counter2 (counter1)" 0 finalValue1
    assertEqual "Try decrementing either counter1 or counter2 (counter2)" 0 finalValue2

  putStrLn "Unit tests completed."


  -- Feature tests
  putStrLn "\n\n"
  -- Create two counters
  counter1 <- newCounter 5
  counter2 <- newCounter 0

  -- Fork a thread to increment counter1
  _ <- forkIO $
    do
      atomically $ incrementCounter counter1
      putStrLn "Incremented counter1."

  -- Fork a thread to decrement counter1 or counter2
  _ <- forkIO $
    do
      atomically $ tryDecrementEither counter1 counter2
      putStrLn "Decremented either counter1 or counter2."

  -- Fork a thread to increment counter2
  _ <- forkIO $
    do
      atomically $ incrementCounter counter2
      putStrLn "Incremented counter2."

  -- Wait for a moment to let other threads complete their actions
  threadDelay 1000000

  -- Print the final state of the counters
  finalValue1 <- atomically $ readTVar counter1
  finalValue2 <- atomically $ readTVar counter2
  putStrLn $ "Final value of counter1: " ++ show finalValue1
  putStrLn $ "Final value of counter2: " ++ show finalValue2

  putStrLn "Feature tests completed."
