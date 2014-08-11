# Monads in OO style in CoffeeScript

# ()
class Unit
UnitObject = new Unit

###
Haskell:

class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

  fmap :: (a -> b) -> m a -> m b
  fmap f ma = ma >>= \a -> return $ f a
###
class Monad
  @make: (a) -> throw new Error("@make unimplemented")
  chain: (fmb) -> throw new Error("chain unimplemented")

  # Default implementation, may be overridden for efficiency.
  map: (f) => this.chain (a) => this.constructor.make f(a)

###
Example: monad-generic function

Haskell syntactic sugar, expanded before compilation:

add aOpt bOpt = do
  a <- aOpt
  b <- bOpt
  return $ a+b

- Monadic context is wrapped in a "do" block.
- Read right to left to see the lambda chains.
- Final arrow with "return" is equivalent to a "map".
###
add = (aOpt, bOpt) ->
  aOpt .chain (a) -> \
  bOpt .map   (b) -> \
  a+b

###
Example: Identity monad

newtype Identity a = Identity a
    deriving Show
           
instance Monad Identity where
  return a = Identity a

  Identity a >>= fmb = fmb a
###
class Identity extends Monad
  constructor: (@a) ->

  @make: (a) -> new Identity(a)

  chain: (fmb) => fmb(@a)
  map: (fab) => Identity.make fab(@a)

###
Example: Option monad

data Option a
  = None
  | Some a
    deriving Show
             
instance Monad Option where
  return a = Some a

  None   >>= _   = None
  Some a >>= fmb = fmb a
###
class Option extends Monad
  @make: (a) -> new Some a

class None extends Option
  chain: (fmb) => this
  map: (fab) => this

NoneObject = new None

class Some extends Option
  constructor: (@a) ->

  chain: (fmb) => fmb(@a)
  map: (fab) => Option.make fab(@a)

###
Example: State monad

newtype State s a = State (s -> (a, s))

instance Monad (State s) where
  return a = State $ \s -> (a, s)

  State runStep >>= fmb = State $ \s ->
    let (a, s') = runStep s
        State runStep' = fmb a
    in
        runStep' s'

runState (State runStep) s =
  let (a, _) = runStep s in a

get :: State a a
get = State $ \s -> (s, s)

put :: s -> State s ()
put s' = State $ \s -> ((), s')
###
class State extends Monad
  constructor: (@runStep) ->

  @make: (a) -> new State (state) -> [a, state]
  @makeLazy: (aThunk) -> new State (state) -> [aThunk(), state]

  chain: (fmb) => new State (state) =>
    [a, newState] = @runStep(state)
    fmb(a).runStep(newState)
  map: (fab) => new State (state) =>
    [a, newState] = @runStep(state)
    [fab(a), newState]
  run: (state) =>
    [a, _] = @runStep(state)
    a
  @get: new State (state) -> [state, state]
  @put: (newState) -> new State (state) -> [UnitObject, newState]

###
Example: computation using state

stateComputation :: State Int Int
stateComputation = do
  n <- get
  put $ 2*n
  n2 <- get
  return $ n2+1
###
stateComputation =
  State.get      .chain (n)  -> \
  State.put(2*n) .chain (_)  -> \
  State.get      .map   (n2) -> \
  n2+1

# Entry point to the real world of effects.
# In Haskell, this is a magical type with no values that
# the compiler optimizes away in code using it. Here we
# have to specify some dummy value.
class RealWorld
TheRealWorld = new RealWorld
  
# Mutable state!
# Fake, to illustrate API: in reality, this is implemented in C.
class MutVar
  @currentIndex: 0
  @vars: []

  constructor: (a) ->
    @index = MutVar.currentIndex
    MutVar.vars[@index] = a
    MutVar.currentIndex++

  read: => MutVar.vars[@index]
  write: (newA) =>
    MutVar.vars[@index] = newA
    UnitObject

###
IO is a specialized State monad in which the state type is RealWorld.
###
class IO extends State
  @make: (aThunk) -> State.makeLazy aThunk

  @newIORef: (a) -> IO.make -> new MutVar(a)
  @readIORef: (v) -> IO.make -> v.read()
  @writeIORef: (v, newA) -> IO.make -> v.write(newA)

  # Fake: all IO actually implemented in C.
  @log: (message) -> IO.make ->
    console.log(message)
    UnitObject

###
import Data.IORef

ioExample :: IO ()
ioExample = do
  putStrLn "Begin mutating"
  x <- newIORef 5
  y <- newIORef 9
  n1 <- readIORef x
  putStrLn $ "x = " ++ show n1
  n2 <- readIORef y
  putStrLn $ "y = " ++ show n2
  writeIORef x $ n1+n2
  n3 <- readIORef x
  n4 <- add (readIORef x) (readIORef y)
  putStrLn $ "x+y = " ++ show n4

Remember that add works for any monad!
### 
ioExample =
  IO.log("Begin mutating").chain (_)  -> \
  IO.newIORef(5)          .chain (x)  -> \
  IO.newIORef(9)          .chain (y)  -> \
  IO.readIORef(x)         .chain (n1) -> \
  IO.log("x = #{n1}")     .chain (_)  -> \
  IO.readIORef(y)         .chain (n2) -> \
  IO.log("y = #{n2}")     .chain (_)  -> \
  IO.writeIORef(x, n1+n2) .chain (_)  -> \
  IO.readIORef(x)         .chain (n3) -> \
  IO.log("x = #{n3}")     .chain (_)  -> \
  add(IO.readIORef(x),
      IO.readIORef(y))    .chain (n4) -> \
  IO.log("x+y = #{n4}")

###
identityExample = do
  putStrLn $ show $ add (return 5 :: Identity Int) (return 6)
###
identityExample =
  IO.log(add(Identity.make(5),
             Identity.make(6)))

###
optionExample = do
  putStrLn $ show $ add (return 5 :: Option Int) (return 6)
  putStrLn $ show $ add (return 5) None
###
optionExample =
  IO.log(add(Option.make(5),
             Option.make(6))) .chain (_) -> \
  IO.log(add(Option.make(5),
             NoneObject))


###
stateExample =
  putStrLn $ show $ runState stateComputation 42
###
stateExample =
  IO.log(stateComputation.run(42))

###
In Haskell, it is required that the program define "main",
an IO state monad object constructed purely.
All effects result from running the object.

main = do
  identityExample
  optionExample
  stateExample
  ioExample
###
main =
  identityExample .chain (_) -> \
  optionExample   .chain (_) -> \
  stateExample    .chain (_) -> \
  ioExample

# In Haskell, the C runtime executes main on a nonexistent "real world",
# performing all effects.
# Fake it here by actually calling it on a fake "real world" object.
_ = main.run(TheRealWorld)
