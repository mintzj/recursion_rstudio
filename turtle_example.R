library(TurtleGraphics)
library(rstackdeque)

turtle_init(mode = "clip")
print(turtle_getpos())

turtle_forward(10)
turtle_turn(45, "left")
turtle_forward(10)


turtle_turn(45, "left")
turtle_up()
turtle_backward(20)
turtle_down()
turtle_backward(10)


print(turtle_getangle())

# returns a vector that remembers the turtles position and angle at the time
# it's called
turtle_getstate <- function() {
  state <- c(turtle_getpos(), turtle_getangle())
  return(state)
}

# sets the turtles position and angle according to a state vector produced by getstate
turtle_setstate <- function(s) {
  turtle_setpos(s[1], s[2])
  turtle_setangle(s[3])
}


####################

turtle_init(mode = "clip")
turtle_hide()

for(i in seq(1,20)) {
  turtle_forward(i)
  turtle_turn(30, "left")
}