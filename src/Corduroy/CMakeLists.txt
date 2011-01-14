flex_target(CorduroyLexer lexer.l lexer.c COMPILE_FLAGS "-i -Pcord_yy")
bison_target(CorduroyParser parser.y parser.c COMPILE_FLAGS "-d -y -pcord_yy")

add_executable(corduroy
  corduroy.cpp debug.cpp error.cpp generator.cpp
  ${FLEX_CorduroyLexer_OUTPUTS} ${BISON_CorduroyParser_OUTPUTS})

target_link_libraries(corduroy gen felt elt mtx)

install(TARGETS corduroy DESTINATION bin)
