-- replit link: https://replitcom/join/bezltfkgka-mdkennedy03
import Control.Concurrent
import Control.Concurrent.STM
import Test.HUnit

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

-- Unit Tests
unitTests :: Test
unitTests = TestList [
    TestCase $ do
      res <- atomically $ newTVar 0
      atomically $ addResource res 10
      finalValue <- atomically $ readTVar res
      assertEqual "addResource should correctly add resources" 10 finalValue,

    TestCase $ do
      res <- atomically $ newTVar 10
      atomically $ consumeResource res 5
      finalValue <- atomically $ readTVar res
      assertEqual "consumeResource should correctly consume resources" 5 finalValue,

    TestCase $ do
      res1 <- atomically $ newTVar 0
      res2 <- atomically $ newTVar 10
      atomically $ nestedTransaction res1 res2
      finalValue1 <- atomically $ readTVar res1
      finalValue2 <- atomically $ readTVar res2
      assertEqual "nestedTransaction should correctly add and consume resources" 10 finalValue1
      assertEqual "nestedTransaction should correctly add and consume resources" 5 finalValue2
  ]


-- Main function to demonstrate nested transactions and conflict handling
main :: IO ()
main = do
  putStrLn "\nRunning Unit Tests"
  _ <- runTestTT unitTests
  putStrLn "\nUnit Tests Completed\n"

  putStrLn "Running Feature Tests\n"

  -- Create two shared resources
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

  putStrLn "Feature Tests Completed\n"