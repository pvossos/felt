add_executable(velvet
  analysis.cpp animate.cpp apply.cpp callbacks.cpp canvas.cpp
  colors.cpp constraint.cpp contour.cpp draw3d.cpp drawingshell.cpp
  element.cpp elementlist.cpp elementop.cpp eps.cpp error.cpp file.cpp
  filedialog.cpp force.cpp generate.cpp globals.cpp graph.cpp grid.cpp group.cpp
  load.cpp loadcase.cpp material.cpp modal.cpp node.cpp nodeop.cpp opengl.cpp
  outputdialog.cpp panel.cpp postscript.cpp ppm.cpp pslib.cpp solution.cpp
  solve.cpp structure.cpp text_entry.cpp text_output.cpp tools.cpp trimesh.cpp
  velvet.cpp vfe.cpp wireframe.cpp)

target_link_libraries(velvet
  widgets gen felt elt mtx
  GLU GL GLw pthread Xaw Xmu Xt SM ICE Xext X11)

install(TARGETS velvet DESTINATION bin)
install(FILES Velvet.ad DESTINATION ${X11DEF_DIR})
