cmake_minimum_required(VERSION 2.8.3)
project(turtle_goal_referee)


find_package(catkin REQUIRED COMPONENTS
  geometry_msgs
  roslisp
  std_msgs
  turtlesim
  )


catkin_package(

)

include_directories(
# include
  ${catkin_INCLUDE_DIRS}
)

add_lisp_executable(referee turtle-goal-referee turtle-goal-referee:referee)
add_lisp_executable(referee-demo turtle-goal-referee turtle-goal-referee:referee-demo)

