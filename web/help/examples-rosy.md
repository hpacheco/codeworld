This is a collection of examples that demonstrate the capabilities of
Rosy.

Example: Blinking led
================

This example demonstrates how you can give your robot memory, and how to use that to make a led blink, alternating between two colors.

~~~~~ . clickable
data Blink = Off | On

blink :: Blink -> (Led1,Blink)
blink Off = (Led1 Black,On)
blink On = (Led1 Red,Off)

main = simulate blink
~~~~~

Example: Random walk
=====================

This example demonstrates how to build a random walker, that makes your robot walk forward, and change course when it finds an obstacle.

~~~~~ . clickable
data Mode = Ok | Panic Clock

-- | the controller is in panic mode during 1 second since the last emergency
mode :: Mode -> Clock -> Mode
mode (Panic old) new = if seconds new-seconds old > 1 then Ok else Panic old
mode Ok _ = Ok 

-- | when the robot has a serious event, signal an emergency
emergency :: Either Bumper Cliff -> Clock -> Mode
emergency _ now = Panic now

-- | move the robot depending on the mode
walk :: Orientation -> Mode -> Velocity
walk (Orientation o) Ok = Velocity 5 0
walk (Orientation o) (Panic _) = Velocity 0 (pi/2)

main = simulate (emergency,mode,walk)
~~~~~

