import Control.Concurrent (forkIO, threadDelay) -- Specify to avoid conflicts with MVar
import Control.Concurrent.STM

-- Define MVar using TVar
-- Concept: IMplementing MVars using TVars An MVar is a mutable variable that can be either empty or full, allowing safe communication between threads
type MVar a = TVar (Maybe a)

-- Create an empty MVar
newEmptyMVar :: STM (MVar a)
newEmptyMVar = newTVar Nothing

-- Put a value into an MVar
putMVar :: MVar a -> a -> STM ()
putMVar mv val = do
  current <- readTVar mv
  case current of
    Nothing -> writeTVar mv (Just val)
    Just _ -> retry -- Block if MVar is already full

-- Take a value from an MVar
takeMVar :: MVar a -> STM a
takeMVar mv = do
  current <- readTVar mv
  case current of
    Nothing -> retry -- Block if MVar is empty
    Just val -> do
      writeTVar mv Nothing -- Empty the MVar
      return val

-- Multicast channel
-- Concept: The paper discusses building multicast channels where messages can be broadcasted to multiple readers
data MChan a = MChan (TVar [TVar (Maybe a)])

-- Create a new multicast channel
newMChan :: STM (MChan a)
newMChan = do
  chan <- newTVar []
  return (MChan chan)

-- Write a value to the multicast channel
writeMChan :: MChan a -> a -> STM ()
writeMChan (MChan chan) val = do
  listeners <- readTVar chan -- reads the current list of listeners (each being a TVar (Maybe a)) from the multicast channel chan
  newListeners <- mapM (\tv -> writeTVar tv (Just val) >> return tv) listeners -- applies the given lambda function to each listener in the listeners list, which writes val into tv and returns the updated TVar tv
  writeTVar chan newListeners -- updates the multicast channel chan with the new list of listeners Even though the TVars themselves haven't changed, writing them back ensures that any transactional modifications are correctly recorded

-- Create a new read port for the multicast channel
newPort :: MChan a -> STM (TVar (Maybe a))
newPort (MChan chan) = do
  port <- newTVar Nothing -- Set port to newTVar with nothing in it
  listeners <- readTVar chan -- This reads the current value of the chan TVar, which holds a list of listeners (each listener is a TVar (Maybe a))
  writeTVar chan (port : listeners) -- This updates the chan TVar to include the new port in the list of listeners The new port is prepended to the existing list of listeners
  return port

-- Read a value from a port
readPort :: TVar (Maybe a) -> STM a
readPort port = do
  val <- readTVar port
  case val of
    Nothing -> retry -- Block if no value is present
    Just v -> do
      writeTVar port Nothing -- Empty the port
      return v

-- Main function to demonstrate the MVar and multicast channel
-- Concept: The examples illustrate how STM enables composable and safe concurrent programming
main :: IO ()
main = do
  putStrLn "\n\n"

  -- Demonstrate MVar
  mv <- atomically newEmptyMVar
  _ <- forkIO $
    do
      atomically $ putMVar mv "Hello, MVar!"
      putStrLn "Put value into MVar"

  _ <- forkIO $
    do
      val <- atomically $ takeMVar mv
      putStrLn $ "Took value from MVar: " ++ val

  -- Wait for the MVar demonstration to complete
  threadDelay 1000000

  -- Demonstrate multicast channel
  chan <- atomically newMChan
  port1 <- atomically $ newPort chan
  port2 <- atomically $ newPort chan

  _ <- forkIO $
    do
      atomically $ writeMChan chan "Hello, MChan!"
      putStrLn "Wrote value to MChan"

  _ <- forkIO $
    do
      val <- atomically $ readPort port1
      putStrLn $ "Port1 received: " ++ val

  _ <- forkIO $
    do
      val <- atomically $ readPort port2
      putStrLn $ "Port2 received: " ++ val

  -- Wait for the multicast channel demonstration to complete
  threadDelay 1000000
