add_executable(feltdriver error.cpp felt.cpp)
target_link_libraries(feltdriver felt elt mtx)

set_target_properties(feltdriver PROPERTIES OUTPUT_NAME felt)

install(TARGETS feltdriver DESTINATION bin)