library(TurtleGraphics)
 
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



simple_tree <- function(size) {
    if(size <= 1) {return()}
  stored_state <- getstate()    # 1
  
  turtle_forward(size)          # 2
  turtle_turn(30, "left")       # 3
  turtle_forward(size*0.5)      # 4
    simple_tree(size*0.65)
  turtle_up()                   # 5
  turtle_backward(size*0.5)     # 6
  turtle_turn(30, "right")      # 7
  turtle_down()
    
  turtle_turn(35, "right")      # 8
  turtle_forward(size*0.5)      # 9
    simple_tree(size*0.65)
  
  setstate(stored_state)        # 10
}









turtle_init(mode = "clip")
turtle_hide()
turtle_setstate(c(50, 0, 0)) # x, y, angle
simple_tree(20)






#turtle_init(mode = "clip")
#turtle_hide()

#for(i in seq(1, 20)) {
#  simple_tree(i)
#  turtle_turn(35)
#  turtle_up()
#  turtle_forward(i)
#  turtle_down()
#}



