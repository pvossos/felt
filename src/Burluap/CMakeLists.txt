include_directories(${LUA_INCLUDE_DIR})
add_library(lfelt MODULE lfelt.cpp)
set_target_properties(lfelt PROPERTIES PREFIX "" OUTPUT_NAME felt)
target_link_libraries(lfelt felt elt mtx)
