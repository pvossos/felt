flex_target(BurlapLexer lexer.l lexer.c COMPILE_FLAGS "-i -Pburlap_yy")
bison_target(BurlapParser parser.y parser.c COMPILE_FLAGS "-d -y -pburlap_yy")

add_executable(burlap
  apply.cpp arithmetic.cpp array.cpp assignment.cpp burlap.cpp
  codegen.cpp coerce.cpp control.cpp debug.cpp descriptor.cpp
  error.cpp execute.cpp fefunc.cpp felt.cpp field.cpp function.cpp
  globals.cpp help.cpp interactive.cpp ${FLEX_BurlapLexer_OUTPUTS}
  ${BISON_BurlapParser_OUTPUTS} literal.cpp location.cpp loop.cpp
  mathfunc.cpp matrixfunc.cpp miscfunc.cpp pathsearch.cpp
  predicate.cpp relational.cpp symbol.cpp trap.cpp vector.cpp write.cpp)

target_link_libraries(burlap felt elt mtx)