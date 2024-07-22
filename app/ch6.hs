-- replit link: https://replitcom/join/bezltfkgka-mdkennedy03
import Control.Concurrent
import Control.Concurrent.STM

-- Define a type for a shared resource
-- Concept: TVars are the basic building blocks of STM, representing mutable shared state that can be accessed and modified within atomic transactions
type Resource = TVar Int

-- Function to add resources
addResource :: Resource -> Int -> STM ()
addResource resource amount = do
  value <- readTVar resource
  writeTVar resource (value + amount)

-- Function to consume resources
-- Concept: STM handles conflicts automatically, retrying transactions that cannot be completed due to concurrent modifications to shared state
consumeResource :: Resource -> Int -> STM ()
consumeResource resource amount = do
  value <- readTVar resource
  if value >= amount
    then writeTVar resource (value - amount)
    else retry -- Block if there are not enough resources

-- Nested transaction example
-- Concept: STM supports nested transactions, where a transaction can contain sub-transactions that can be executed atomically within the parent transaction
nestedTransaction :: Resource -> Resource -> STM ()
nestedTransaction res1 res2 = do
  -- Nested transaction to add to the first resource and consume from the second
  addResource res1 10
  consumeResource res2 5

-- Main function to demonstrate nested transactions and conflict handling
main :: IO ()
main = do
  putStrLn "\n\n"
  -- Create two shared resources
  -- Concept: STM ensures that a series of memory operations within a transaction are executed atomically, meaning all changes are applied together or not at all
  res1 <- atomically $ newTVar 100
  res2 <- atomically $ newTVar 50

  -- Fork a thread to perform a nested transaction
  _ <- forkIO $
    do
      atomically $ nestedTransaction res1 res2
      putStrLn "Nested transaction completed"

  -- Fork another thread to add resources to res2
  _ <- forkIO $
    do
      atomically $ addResource res2 20
      putStrLn "Added resources to res2"

  -- Fork another thread to consume resources from res1
  _ <- forkIO $
    do
      atomically $ consumeResource res1 30
      putStrLn "Consumed resources from res1"

  -- Wait for a moment to let other threads complete their actions
  threadDelay 1000000

  -- Print the final state of the resources
  finalValue1 <- atomically $ readTVar res1
  finalValue2 <- atomically $ readTVar res2
  putStrLn $ "Final value of res1: " ++ show finalValue1
  putStrLn $ "Final value of res2: " ++ show finalValue2
