add_executable(patchwork
  dxf.cpp dxflib.cpp error.cpp graph.cpp logan.cpp
  oogl.cpp patchwork.cpp util.cpp)
target_link_libraries(patchwork felt elt mtx gen)