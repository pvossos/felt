add_executable(loom
  error.cpp loom.cpp options.cpp graph.cpp wireframe.cpp draw.cpp
  draw3d.cpp contour.cpp bivar.cpp bmp.cpp)
target_link_libraries(loom felt elt mtx X11)
