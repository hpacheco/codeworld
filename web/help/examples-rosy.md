This is a collection of examples that demonstrate the capabilities of
Rosy.

Example: Move forward
================

This example makes a robot move forward at a constant velocity of 0.5 m/s.

~~~~~ . clickable
move :: Velocity
move = Velocity 0.5 0

main = simulate move
~~~~~

Example: Accelerate forward
================

This example makes a robot accelerate forward with non-constant velocity.

~~~~~ . clickable
accelerate :: Velocity -> Velocity
accelerate (Velocity vl va) = Velocity (vl+0.5) va

main = simulate accelerate
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

main = simulate accelerateAndPlay
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

main = simulate forwardBackward
~~~~~

Example: Blinking led
================

This example demonstrates how you can give your robot memory, and how to use that to make a led blink, alternating between two colors.

~~~~~ . clickable
data Blink = Off | On

blink :: Memory Blink -> (Led1,Memory Blink)
blink (Memory Off) = (Led1 Black,Memory On)
blink (Memory On) = (Led1 Red,Memory Off)

main = simulate blink
~~~~~

Example: Random walker
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

main = simulate randomWalk
~~~~~

Example: Kobuki Random walker
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

chgdir :: ChgDir -> StdGen -> Seconds
       -> Memory Mode
chgdir _ r now = Memory (Turn dir time)
    where
    (b,r') = random r
    (ang,_) = randomR (0,pi) r'
    dir = if b then 1 else -1
    time = now + doubleToSeconds (ang / 0.1)

spin :: Memory Mode -> Seconds
     -> (Velocity,Memory Mode)
spin m@(Memory Stop) _ = (Velocity 0 0,m)
spin m@(Memory (Turn dir t)) now | t > now =
  (Velocity 0 (dir*0.1),m)
spin m _ = (Velocity 0.5 0,Memory Go)

randomWalk = (bumper,cliff,wheel,chgdir,spin)

main = simulate randomWalk
~~~~~

