flex_target(FeltLexer lexer.l lexer.c COMPILE_FLAGS "-i -Pfelt_yy")
bison_target(FeltParser parser.y parser.cpp COMPILE_FLAGS "-d -y -pfelt_yy")
add_library(felt
         code.cpp definition.cpp detail.cpp draw.cpp
         fe.cpp file.cpp initialize.cpp ${FLEX_FeltLexer_OUTPUTS} modal.cpp
         nonlinear.cpp objects.cpp ${BISON_FeltParser_OUTPUTS} problem.cpp
         renumber.cpp results.cpp rosenbrock.cpp spectral.cpp transient.cpp)

