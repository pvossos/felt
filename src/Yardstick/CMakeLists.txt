add_executable(yardstick error.cpp scale.cpp yardstick.cpp)

target_link_libraries(yardstick felt elt mtx)

install(TARGETS yardstick DESTINATION bin)

