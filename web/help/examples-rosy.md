This is a collection of examples that demonstrate the capabilities of
Rosy.

# Kobuki

Example: Move Kobuki forward
================

This example makes a robot move forward at a constant velocity of 0.5 m/s.

~~~~~ . clickable
move :: Velocity
move = Velocity 0.5 0

main = simulate (Kobuki Nothing) move
~~~~~

Example: Accelerate forward
================

This example makes a robot accelerate forward with non-constant velocity.

~~~~~ . clickable
accelerate :: Velocity -> Velocity
accelerate (Velocity vl va) = Velocity (vl+0.5) va

main = simulate (Kobuki Nothing) accelerate
~~~~~

Example: Accelerate forward and play a sound on collision
================

This example makes a robot accelerate forward with non-constant velocity, and play a sound when it hits a wall.

~~~~~ . clickable
accelerate :: Velocity -> Velocity
accelerate (Velocity vl va) = Velocity (vl+0.5) va

play :: Bumper -> Maybe Sound
play (Bumper _ Pressed)  = Just ErrorSound
play (Bumper _ Released) = Nothing

accelerateAndPlay = (accelerate,play)

main = simulate (Kobuki Nothing) accelerateAndPlay
~~~~~

Example: Accelerate forward and backwards
================

This example makes a robot accelerate forward, and reverse its direction when it hits a wall.

~~~~~ . clickable
type Hit = Bool

reverseDir :: Bumper -> Memory Hit
reverseDir _ = Memory True

accelerate :: Memory Hit -> Velocity -> Velocity
accelerate (Memory hit) (Velocity vl va) = if hit
    then Velocity (vl-0.5) va
    else Velocity (vl+0.5) va
    
forwardBackward = (reverseDir,accelerate)

main = simulate (Kobuki Nothing) forwardBackward
~~~~~

Example: Blinking led
================

This example demonstrates how you can give your robot memory, and how to use that to make a led blink, alternating between two colors.

~~~~~ . clickable
data Blink = BOff | BOn

blink :: Memory Blink -> (Led1,Memory Blink)
blink (Memory BOff) = (Led1 Black,Memory BOn)
blink (Memory BOn) = (Led1 Red,Memory BOff)

main = simulate (Kobuki Nothing) blink
~~~~~

Example: Simple Random Walker
=====================

This example demonstrates how to implement a simple random walker, that makes your robot walk forward, and change course when it finds an obstacle.

~~~~~ . clickable
data Mode = Ok | Panic Clock

-- | the controller is in panic mode during 1 second since the last emergency
mode :: Memory Mode -> Clock -> Memory Mode
mode (Memory (Panic old)) new = if seconds new-seconds old > 1 then Memory Ok else Memory (Panic old)
mode (Memory Ok) _ = Memory Ok 

-- | when the robot has a serious event, signal an emergency
emergency :: Either Bumper Cliff -> Clock -> Memory Mode
emergency _ now = Memory (Panic now)

-- | move the robot depending on the mode
walk :: Orientation -> Memory Mode -> Velocity
walk (Orientation o) (Memory Ok) = Velocity 0.5 0
walk (Orientation o) (Memory (Panic _)) = Velocity 0 (pi/8)

randomWalk = (emergency,mode,walk)

main = simulate (Kobuki Nothing) randomWalk
~~~~~

Example: Kobuki Random Walker
=====================

This example demonstrates how to replicate a Kobuki random walker, with blinking leds and randomized behavior.

~~~~~ . clickable
data Mode = Go | Stop | Turn Double Seconds
data ChgDir = ChgDir -- change direction

bumper :: Bumper -> (Led1,Maybe ChgDir)
bumper (Bumper _ st) = case st of
  Pressed -> (Led1 Orange,Just ChgDir)
  Released -> (Led1 Black,Nothing)

cliff :: Cliff -> (Led2,Maybe ChgDir)
cliff (Cliff _ st) = case st of
  Hole -> (Led2 Orange,Just ChgDir)
  Floor -> (Led2 Black,Nothing)

wheel :: Wheel -> (Led1,Led2,Memory Mode)
wheel (Wheel _ st) = case st of
  Air -> (Led1 Red,Led2 Red,Memory Stop)
  Ground -> (Led1 Black,Led2 Black,Memory Go)

chgdir :: ChgDir -> StdGen -> Seconds -> Memory Mode
chgdir _ r now = Memory (Turn dir time)
    where
    (b,r') = random r
    (ang,_) = randomR (0,pi) r'
    dir = if b then 1 else -1
    time = now + doubleToSeconds (ang / 0.1)

spin :: Memory Mode -> Seconds -> (Velocity,Memory Mode)
spin m@(Memory Stop) _ = (Velocity 0 0,m)
spin m@(Memory (Turn dir t)) now | t > now = (Velocity 0 (dir*0.1),m)
spin m _ = (Velocity 0.5 0,Memory Go)

randomWalk = (bumper,cliff,wheel,chgdir,spin)

main = simulate (Kobuki Nothing) randomWalk
~~~~~

Example: Kobuki Random Walker with Safety Controller
=====================

This example demonstrates how to encode a multiplexer, in order to combine the Kobuki random walker and the Kobuki safety controller.

~~~~~ . clickable
-- random walker

data Mode = Go | Stop | Turn Double Seconds
data ChgDir = ChgDir -- change direction

bumper :: Bumper -> (Led1,Maybe ChgDir)
bumper (Bumper _ st) = case st of
  Pressed -> (Led1 Orange,Just ChgDir)
  Released -> (Led1 Black,Nothing)

cliff :: Cliff -> (Led2,Maybe ChgDir)
cliff (Cliff _ st) = case st of
  Hole -> (Led2 Orange,Just ChgDir)
  Floor -> (Led2 Black,Nothing)

wheel :: Wheel -> (Led1,Led2,Memory Mode)
wheel (Wheel _ st) = case st of
  Air -> (Led1 Red,Led2 Red,Memory Stop)
  Ground -> (Led1 Black,Led2 Black,Memory Go)

chgdir :: ChgDir -> StdGen -> Seconds
       -> Memory Mode
chgdir _ r now = Memory (Turn dir time)
    where
    (b,r') = random r
    (ang,_) = randomR (0,pi) r'
    dir = if b then 1 else -1
    time = now + doubleToSeconds (ang / 0.1)

spin :: Memory Mode -> Seconds -> (M2 Velocity,Memory Mode)
spin m@(Memory Stop) _ = (M2 (Velocity 0 0),m)
spin m@(Memory (Turn dir t)) now | t > now = (M2 (Velocity 0 (dir*0.1)),m)
spin m _ = (M2 (Velocity 0.5 0),Memory Go)

randomWalk = (bumper,cliff,wheel,chgdir,spin)

-- safety controller

safetyControl :: Either (Either Bumper Cliff) Wheel -> Maybe (M1 Velocity)
safetyControl (Right (Wheel _ Air)) = Just $ M1 $ Velocity 0 0
safetyControl (Left (Left (Bumper CenterBumper Pressed))) = Just $ M1 $ Velocity (-0.1) 0
safetyControl (Left (Right (Cliff CenterCliff Hole))) = Just $ M1 $ Velocity (-0.1) 0
safetyControl (Left (Left (Bumper LeftBumper Pressed))) = Just $ M1 $ Velocity (-0.1) (-0.4)
safetyControl (Left (Right (Cliff LeftCliff Hole))) = Just $ M1 $ Velocity (-0.1) (-0.4)
safetyControl (Left (Left (Bumper RightBumper Pressed))) = Just $ M1 $ Velocity (-0.1) 0.4
safetyControl (Left (Right (Cliff RightCliff Hole))) = Just $ M1 $ Velocity (-0.1) 0.4
safetyControl _ = Nothing

-- multiplexer

data M = Start | Ignore Seconds
data M1 a = M1 a
data M2 b = M2 b

timeout = 0.5

muxVel :: Seconds -> Memory M
    -> Either (M1 Velocity) (M2 Velocity) -> Maybe (Velocity,Memory M)
muxVel t _ (Left (M1 a)) = Just (a,Memory (Ignore (t+timeout)))
muxVel t (Memory (Ignore s)) (Right (M2 a)) | s > t = Nothing
muxVel t _ (Right (M2 a)) = Just (a,Memory Start)

-- safe random walker
    
safeRandomWalk = (randomWalk,safetyControl,muxVel)

main = simulate (Kobuki Nothing) safeRandomWalk
~~~~~

Example: Task - Turn left or right by a number of degrees
=====================

This example demonstrates how to implement a simple task that makes the robot rotate to the left or to the right.

~~~~~ . clickable
type Side = Either Degrees Degrees

turn :: Side -> Task () ()
turn s = task (startTurn s) runTurn

startTurn :: Side -> Orientation -> Memory Orientation
startTurn (Left a)  o = Memory (o+degreesToOrientation a)
startTurn (Right a) o = Memory (o-degreesToOrientation a)

errTurn = 0.01

runTurn :: Memory Orientation -> Orientation
        -> Either (Velocity) (Done ())
runTurn (Memory to) from = if abs d <= errTurn
    then Right (Done ())
    else Left (Velocity 0 (orientation d))
  where d = normOrientation (to-from)
    
main = simulate (Kobuki Nothing) (turn $ Left 90)
~~~~~

Example: Task - Move forward or backward for a number of centimeters
=====================

This example demonstrates how to implement a simple task that makes the robot move forward or backward.

~~~~~ . clickable
data Direction = Forward Centimeters | Backward Centimeters

move :: Direction -> Task () ()
move d = task (startMove d) runMove

startMove :: Direction -> Position -> Memory Position
startMove (Forward cm) p = Memory $ vecToPosition $ extVec (positionToVec p) $ centimetersToMeters cm
startMove (Backward cm) p = Memory $ vecToPosition $ extVec (positionToVec p) $ centimetersToMeters (-cm)

errMove = 0.1

runMove :: Memory Position -> Position -> Either Velocity (Done ())
runMove (Memory to) from = if abs dist <= errMove
    then Right (Done ())
    else Left (Velocity dist 0)
  where dist = magnitudeVec (subVec (positionToVec to) (positionToVec from))

main = simulate (Kobuki $ Just world2) (move $ Forward 32)
~~~~~

Example: Task - Draw a square
=====================

This example demonstrates how to make the robot draw a square with his movement.

~~~~~ . clickable

-- turn left/right

type Side = Either Degrees Degrees

turn :: Side -> Task () ()
turn s = task (startTurn s) runTurn

startTurn :: Side -> Orientation -> Memory Orientation
startTurn (Left a)  o = Memory (o+degreesToOrientation a)
startTurn (Right a) o = Memory (o-degreesToOrientation a)

errTurn = 0.01

runTurn :: Memory Orientation -> Orientation -> Either (Velocity) (Done ())
runTurn (Memory to) from = if abs d <= errTurn
    then Right (Done ())
    else Left (Velocity 0 (orientation d))
  where d = normOrientation (to-from)

-- task move

data Direction = Forward Centimeters | Backward Centimeters

move :: Direction -> Task () ()
move d = task (startMove d) runMove

startMove :: Direction -> Orientation -> Position -> Memory Position
startMove d (Orientation angle) p = Memory $ vecToPosition $ addVec (positionToVec p) $ scalarVec (magnitude d) angle
    where
    magnitude (Forward cm) = centimetersToMeters cm
    magnitude (Backward cm) = - centimetersToMeters cm

errMove = 0.01

runMove :: Memory Position -> Position -> Either Velocity (Done ())
runMove (Memory to) from = if abs dist <= errMove
    then Right (Done ())
    else Left (Velocity dist 0)
  where dist = magnitudeVec (subVec (positionToVec to) (positionToVec from))

mainM = simulate (Kobuki $ Just world2) (move $ Forward 32)

-- draw square

drawSquare :: Task () ()
drawSquare = replicateM_ 4 $ do
    move (Forward 32)
    turn (Left 90)
    
main = simulate (Kobuki $ Just world2) drawSquare
~~~~~

# Turtlesim

Example: Task - Turn left or right by a number of degrees
=====================

This example demonstrates how to implement a simple task that makes the robot rotate to the left or to the right.

~~~~~ . clickable
type Side = Either Degrees Degrees
    
startTurn :: Turtle n () -> Side -> Turtle n Orientation -> (Memory Orientation)
startTurn _ ang (Turtle o) = (Memory $ o+degreesToOrientation a)
  where a = case ang of { Left a -> a; Right a -> -a }

errTurn = 0.01

runTurn :: Turtle n () -> Memory Orientation -> Turtle n Orientation -> Either (Turtle n Velocity) (Turtle n Velocity,Done ())
runTurn _ (Memory to) (Turtle from) = if abs d <= errTurn
    then Right (Turtle $ Velocity 0 0,Done ())
    else Left (Turtle $ Velocity 0 $ orientation d)
  where d = normOrientation (to-from)

turn :: TurtleNumber -> Side -> Task () ()
turn n m = onTurtle n $ \t -> task (startTurn t m) (runTurn t)
    
main = simulate Turtlesim (turn 1 $ Left 90)
~~~~~


Example: Task - Move forward or backward for a number of centimeters
=====================

This example demonstrates how to implement a simple task that makes the Turtlesim robot move forward or backward.

~~~~~ . clickable
data Direction = Forward Centimeters | Backward Centimeters

startMove :: Turtle n () -> Direction -> Turtle n Position -> Turtle n Orientation -> (Memory Position,Memory Orientation)
startMove _ dist (Turtle pos) (Turtle o@(Orientation ang)) = (Memory dest,Memory $ normOrientation o)
    where
    dest = vecToPosition $ addVec (positionToVec pos) (scalarVec d ang)
    d = case dist of { Forward d -> d; Backward d -> -d }

errMove = 0.01

runMove :: Turtle n () -> Turtle n Position -> Memory Position -> Memory Orientation -> Either (Turtle n Velocity) (Turtle n Velocity,Done ())
runMove _ (Turtle now) (Memory dest) (Memory o) = if dist <= errMove
    then Right (Turtle $ Velocity 0 0,Done ())
    else Left (Turtle $ Velocity vel 0)
  where
  diff = subVec (positionToVec dest) (positionToVec now)
  dist = magnitudeVec diff
  ang = angleVec diff
  vel = if abs (normOrientation (Orientation ang-o)) >= Orientation pi/2 then -dist else dist
  
move :: TurtleNumber -> Direction -> Task () ()
move n m = onTurtle n $ \t -> task (startMove t m) (runMove t)

main = simulate Turtlesim (move 1 $ Forward 1)
~~~~~

Example: Task - Draw a spiral with a color gradient
=====================

This example demonstrates how to make a turtle draw a spiral with growing thickness and color.

~~~~~ . clickable
type Side = Either Degrees Degrees
    
startTurn :: Turtle n () -> Side -> Turtle n Orientation -> (Memory Orientation)
startTurn _ ang (Turtle o) = (Memory $ o+degreesToOrientation a)
  where a = case ang of { Left a -> a; Right a -> -a }

errTurn = 0.01

runTurn :: Turtle n () -> Memory Orientation -> Turtle n Orientation -> Either (Turtle n Velocity) (Turtle n Velocity,Done ())
runTurn _ (Memory to) (Turtle from) = if abs d <= errTurn
    then Right (Turtle $ Velocity 0 0,Done ())
    else Left (Turtle $ Velocity 0 $ orientation d)
  where d = normOrientation (to-from)

turn :: TurtleNumber -> Side -> Task () ()
turn n m = onTurtle n $ \t -> task (startTurn t m) (runTurn t)
    
----

data Direction = Forward Centimeters | Backward Centimeters

startMove :: Turtle n () -> Direction -> Turtle n Position -> Turtle n Orientation -> (Memory Position,Memory Orientation)
startMove _ dist (Turtle pos) (Turtle o@(Orientation ang)) = (Memory dest,Memory $ normOrientation o)
    where
    dest = vecToPosition $ addVec (positionToVec pos) (scalarVec d ang)
    d = case dist of { Forward d -> d; Backward d -> -d }

errMove = 0.01

runMove :: Turtle n () -> Turtle n Position -> Memory Position -> Memory Orientation -> Either (Turtle n Velocity) (Turtle n Velocity,Done ())
runMove _ (Turtle now) (Memory dest) (Memory o) = if dist <= errMove
    then Right (Turtle $ Velocity 0 0,Done ())
    else Left (Turtle $ Velocity vel 0)
  where
  diff = subVec (positionToVec dest) (positionToVec now)
  dist = magnitudeVec diff
  ang = angleVec diff
  vel = if abs (normOrientation (Orientation ang-o)) >= Orientation pi/2 then -dist else dist
  
move :: TurtleNumber -> Direction -> Task () ()
move n m = onTurtle n $ \t -> task (startMove t m) (runMove t)

----

spiral :: TurtleNumber -> Double -> Double -> Double -> Color -> (Color -> Color) -> Task () ()
spiral n len ang width c upd = do
    setPen n (Pen c (floor width) On)
    move n (Forward len)
    turn n (Left ang)
    spiral n (len+0.02) (ang-0.5) (width+0.2) (upd c) upd

red_spiral1 = spiral 1 0.2 30 1 black tored
    where black = Color 0 0 0
          tored (Color r g b) = Color (r+10) g b
    
main = simulate Turtlesim red_spiral1
~~~~~

Example: Task - Draw two spirals with different color gradients
=====================

This example demonstrates how to make two turtles draw two spirals with growing thickness and color.

~~~~~ . clickable
type Side = Either Degrees Degrees
    
startTurn :: Turtle n () -> Side -> Turtle n Orientation -> (Memory Orientation)
startTurn _ ang (Turtle o) = (Memory $ o+degreesToOrientation a)
  where a = case ang of { Left a -> a; Right a -> -a }

errTurn = 0.01

runTurn :: Turtle n () -> Memory Orientation -> Turtle n Orientation -> Either (Turtle n Velocity) (Turtle n Velocity,Done ())
runTurn _ (Memory to) (Turtle from) = if abs d <= errTurn
    then Right (Turtle $ Velocity 0 0,Done ())
    else Left (Turtle $ Velocity 0 $ orientation d)
  where d = normOrientation (to-from)

turn :: TurtleNumber -> Side -> Task () ()
turn n m = onTurtle n $ \t -> task (startTurn t m) (runTurn t)
    
----

data Direction = Forward Centimeters | Backward Centimeters

startMove :: Turtle n () -> Direction -> Turtle n Position -> Turtle n Orientation -> (Memory Position,Memory Orientation)
startMove _ dist (Turtle pos) (Turtle o@(Orientation ang)) = (Memory dest,Memory $ normOrientation o)
    where
    dest = vecToPosition $ addVec (positionToVec pos) (scalarVec d ang)
    d = case dist of { Forward d -> d; Backward d -> -d }

errMove = 0.01

runMove :: Turtle n () -> Turtle n Position -> Memory Position -> Memory Orientation -> Either (Turtle n Velocity) (Turtle n Velocity,Done ())
runMove _ (Turtle now) (Memory dest) (Memory o) = if dist <= errMove
    then Right (Turtle $ Velocity 0 0,Done ())
    else Left (Turtle $ Velocity vel 0)
  where
  diff = subVec (positionToVec dest) (positionToVec now)
  dist = magnitudeVec diff
  ang = angleVec diff
  vel = if abs (normOrientation (Orientation ang-o)) >= Orientation pi/2 then -dist else dist
  
move :: TurtleNumber -> Direction -> Task () ()
move n m = onTurtle n $ \t -> task (startMove t m) (runMove t)

----

spiral :: TurtleNumber -> Double -> Double -> Double -> Color -> (Color -> Color) -> Task () ()
spiral n len ang width c upd = do
    setPen n (Pen c (floor width) On)
    move n (Forward len)
    turn n (Left ang)
    spiral n (len+0.02) (ang-0.5) (width+0.2) (upd c) upd

red_spiral1 = spiral 1 0.2 30 1 black tored
    where black = Color 0 0 0
          tored (Color r g b) = Color (r+10) g b
          
green_spiral2 = do
    turtle <- spawn turtlesimDefaultPosition turtlesimDefaultOrientation
    spiral turtle 0.3 40 1 black togreen
  where
    black = Color 0 0 0
    togreen (Color r g b) = Color r (g+10) b
    
spiral12 = (call red_spiral1 noCancel noFeedback id,call green_spiral2 noCancel noFeedback id)
    
main = simulate Turtlesim spiral12
~~~~~

Example: Task - Draw a fractal tree with turtles as leaves
=====================

This example demonstrates how to make multiple turtles draw a fractal tree.

~~~~~ . clickable
type Side = Either Degrees Degrees
    
startTurn :: Turtle n () -> Side -> Turtle n Orientation -> (Memory Orientation)
startTurn _ ang (Turtle o) = (Memory $ o+degreesToOrientation a)
  where a = case ang of { Left a -> a; Right a -> -a }

errTurn = 0.01

runTurn :: Turtle n () -> Memory Orientation -> Turtle n Orientation -> Either (Turtle n Velocity) (Turtle n Velocity,Done ())
runTurn _ (Memory to) (Turtle from) = if abs d <= errTurn
    then Right (Turtle $ Velocity 0 0,Done ())
    else Left (Turtle $ Velocity 0 $ orientation d)
  where d = normOrientation (to-from)

turn :: TurtleNumber -> Side -> Task () ()
turn n m = onTurtle n $ \t -> task (startTurn t m) (runTurn t)
    
----

data Direction = Forward Centimeters | Backward Centimeters

startMove :: Turtle n () -> Direction -> Turtle n Position -> Turtle n Orientation -> (Memory Position,Memory Orientation)
startMove _ dist (Turtle pos) (Turtle o@(Orientation ang)) = (Memory dest,Memory $ normOrientation o)
    where
    dest = vecToPosition $ addVec (positionToVec pos) (scalarVec d ang)
    d = case dist of { Forward d -> d; Backward d -> -d }

errMove = 0.01

runMove :: Turtle n () -> Turtle n Position -> Memory Position -> Memory Orientation -> Either (Turtle n Velocity) (Turtle n Velocity,Done ())
runMove _ (Turtle now) (Memory dest) (Memory o) = if dist <= errMove
    then Right (Turtle $ Velocity 0 0,Done ())
    else Left (Turtle $ Velocity vel 0)
  where
  diff = subVec (positionToVec dest) (positionToVec now)
  dist = magnitudeVec diff
  ang = angleVec diff
  vel = if abs (normOrientation (Orientation ang-o)) >= Orientation pi/2 then -dist else dist
  
move :: TurtleNumber -> Direction -> Task () ()
move n m = onTurtle n $ \t -> task (startMove t m) (runMove t)

----

getLocation :: Turtle n () -> Turtle n Position -> Turtle n Orientation -> Done (Position,Orientation)
getLocation _ (Turtle p) (Turtle n) = Done (p,n)

location :: TurtleNumber -> Task () (Position,Orientation)
location n = onTurtle n $ \t -> task () (getLocation t)

----

angle = 30
rads :: Orientation
rads = Orientation $ degreesToRadians angle

tree :: Int -> TurtleNumber -> Double -> Task () ()
tree 0 turtle branch = return ()
tree n turtle branch = do
    move turtle (Forward branch)
    (pos,ori) <- location turtle
    let left = do
            tree (n-1) turtle (branch/2)
    let right = do
            turtle <- spawn pos (ori-rads)
            tree (n-1) turtle (branch/2)
    when (n > 1) $ do
        turn turtle (Left angle)
        parallel_ left right >> return ()

reposition :: Task () ()
reposition = do
    kill 1
    turtle <- spawn (Position 5.5 0) (Orientation $ pi/2)
    return ()

main = simulate Turtlesim (reposition >> tree 3 1 3)
~~~~~


