add_executable(velvet
  analysis.c animate.cpp apply.c callbacks.c canvas.c
  colors.c constraint.c contour.c draw3d.c drawingshell.c
  element.c elementlist.c elementop.c eps.c error.c file.c
  filedialog.c force.c generate.cpp globals.c graph.cpp grid.c group.c
  load.c loadcase.c material.c modal.cpp node.c nodeop.c opengl.c
  outputdialog.c panel.c postscript.c ppm.c pslib.c solution.c
  solve.cpp structure.cpp text_entry.c text_output.c tools.c trimesh.c
  velvet.c vfe.c wireframe.c)

target_link_libraries(velvet
  widgets gen felt elt mtx
  GLU GL GLw pthread Xaw Xmu Xt SM ICE Xext X11)

install(TARGETS velvet DESTINATION bin)
install(FILES Velvet.ad DESTINATION ${X11DEF_DIR})
