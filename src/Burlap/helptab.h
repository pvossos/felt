/*
    This file is part of the FElt finite element analysis package.
    Copyright (C) 1993-2000 Jason I. Gobat and Darren C. Atkinson

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/
static char help_0 [ ] ="\
assemble ( )\n\
assemble (M, C)\n\
\n\
    Computes and returns the global stiffness matrix by computing the local\n\
    stiffness matrices and assembling them into the global matrix.  For\n\
    transient problems, if M is specified then it will contain the global\n\
    mass matrix on return.  Similarly, C will contain the global damping\n\
    matrix.  All matrices are compact.\n\
";

static char help_1 [ ] ="\
clear_nodes ( )\n\
\n\
    Clears the displacements and equivalent nodal force vectors for all\n\
    nodes in the current problem.\n\
";

static char help_2 [ ] ="\
compute_modes (K, M)\n\
compute_modes (K, M, X)\n\
\n\
    Compute the modes for the given stiffness matrix, K, and mass matrix, M.\n\
    The result is the vector of eigenvalues.  If X is specified then it will\n\
    contain the matrix of eigenvectors upon return.\n\
";

static char help_3 [ ] ="\
compute_stresses (e)\n\
\n\
    Not available yet.\n\
";

static char help_4 [ ] ="\
construct_forces ( )\n\
construct_forces (t)\n\
\n\
    Constructs and returns the global nodal force vector based on all nodal\n\
    forces and the global DOFs active at those nodes.  For transient\n\
    problems, t may be a scalar expression used to specify the current time.\n\
    If t is missing then it is assumed to be zero.\n\
";

static char help_5 [ ] ="\
find_dofs ( )\n\
\n\
    Computes the set of active DOFs for the current problem.  As a result,\n\
    the DOF-related fields of the problem structure are initialized.  The\n\
    number of active DOFs is returned.\n\
";

static char help_6 [ ] ="\
global_dof (n, d)\n\
\n\
    Returns the global DOF corresponding to a local DOF.  The local DOF is\n\
    specified by its node, n, and the DOF, d.  The node may be specified as\n\
    either a node object or a node number.\n\
";

static char help_7 [ ] ="\
integrate_hyperbolic (K, M, C)\n\
\n\
    Solves the discrete equation of motion, Ma + Cv + Kd = F, using\n\
    Newmark's method with the Hilbert-Hughes-Taylor alpha correction for\n\
    improved accuracy with numerical damping.  The result is a matrix of\n\
    nodal displacements, with each column corresponding to a single time\n\
    step.  The sizes of the matrices must be consistent with the definition\n\
    of the problem.  Compact matrices are expected.\n\
";

static char help_8 [ ] ="\
integrate_parabolic (K, M)\n\
\n\
    Solves the discrete parabolic differential equation Mv + Kd = F using a\n\
    generalized trapezoidal method.  The sizes of the matrices must be \n\
    consistent with the definition of the problem.  Compact matrices are\n\
    expected.\n\
";

static char help_9 [ ] ="\
local_dof (g)\n\
local_dof (g, l)\n\
\n\
    Returns the number of the node corresponding to the global DOF, g.  If l\n\
    is specified the it will contain the local DOF on return.  (The number\n\
    of the node is returned rather than the node object itself since the\n\
    nodes may have been renumbered.)\n\
";

static char help_10 [ ] ="\
remove_constrained (K)\n\
\n\
    Removes the rows and columns of K at all DOFs with a fixed boundary\n\
    condition and returns the new matrix.  K is not modified.  K should with\n\
    be either a symmetric matrix or a column vector.  The size of K must be\n\
    consistent with the definition of the problem.\n\
";

static char help_11 [ ] ="\
renumber_nodes ( )\n\
\n\
    Renumbers the nodes of the current problem using the\n\
    Gibbs-Poole-Stockmeyer and Gibbs-King node renumbering algorithms for\n\
    bandwidth and profile reduction.  The result is a permutation vector of\n\
    the node numbers.\n\
";

static char help_12 [ ] ="\
restore_numbers (p)\n\
\n\
    Restores the original node numbering of the current problem.  The\n\
    permutation vector is specified by p.  The return value is always null.\n\
";

static char help_13 [ ] ="\
set_up (e)\n\
set_up (e, s)\n\
\n\
    Not available yet.\n\
";

static char help_14 [ ] ="\
solve_displacements (K, f)\n\
\n\
    Solves the linear system Kd = f for the vector of global nodal\n\
    displacements.  The sizes of the inputs must be consistent with the\n\
    definition of the problem.  Additionally, K and f should both be\n\
    condensed.  K is expected to be compact.\n\
";

static char help_15 [ ] ="\
zero_constrained (K)\n\
\n\
    Zeroes the rows and columns of K at all DOFs with a fixed boundary\n\
    condition and returns the new matrix.  K is not modified.  K should with\n\
    be either a symmetric matrix or a column vector.  If K is a matrix then\n\
    a one is placed on the corresponding diagonal.  The size of K must be\n\
    consistent with the definition of the problem.\n\
";

static char help_16 [ ] ="\
abs (X)\n\
fabs (X)\n\
\n\
    Computes the absolute value of each element of X.  If X is a scalar then\n\
    the result is a scalar.  If X is a matrix then the result is a matrix.\n\
";

static char help_17 [ ] ="\
ceil (X)\n\
\n\
    Computes the ceiling of each element of X.  If X is a scalar then the\n\
    result is a scalar.  If X is a matrix then the result is a matrix.\n\
";

static char help_18 [ ] ="\
cos (X)\n\
\n\
    Computes the cosine of each element of X.  If X is a scalar then the\n\
    result is a scalar.  If X is a matrix then the result is a matrix.\n\
";

static char help_19 [ ] ="\
exp (X)\n\
\n\
    Computes the exponential of each element of X (e raised to the power X).\n\
    If X is a scalar then the result is a scalar.  If X is a matrix then the\n\
    result is a matrix.\n\
";

static char help_20 [ ] ="\
floor (X)\n\
\n\
    Computes the floor of each element of X.  If X is a scalar then the\n\
    result is a scalar.  If X is a matrix then the result is a matrix.\n\
";

static char help_21 [ ] ="\
hypot (X, Y)\n\
\n\
    Computes the square root of X*X+Y*Y.  If X and Y represent the lengths\n\
    of the sides of a right triangle, then the result is the length of the\n\
    hypotenuse.  If X and Y are both scalars then the result is a scalar.\n\
    If X and Y are both matrices of the same size then the result is a\n\
    matrix.\n\
";

static char help_22 [ ] ="\
log (X)\n\
\n\
    Computes the natural logarithm of each element of X.  If X is a scalar\n\
    then the result is a scalar.  If X is a matrix then the result is a\n\
    matrix.\n\
";

static char help_23 [ ] ="\
log10 (X)\n\
\n\
    Computes the base-10 logarithm of each element of X.  If X is a scalar\n\
    then the result is a scalar.  If X is a matrix then the result is a\n\
    matrix.\n\
";

static char help_24 [ ] ="\
sin (X)\n\
\n\
    Computes the sine of each element of X.  If X is a scalar then the\n\
    result is a scalar.  If X is a matrix then the result is a matrix.\n\
";

static char help_25 [ ] ="\
sqrt (X)\n\
\n\
    Computes the square root of each element of X.  If X is a scalar then\n\
    the result is a scalar.  If X is a matrix then the result is a matrix.\n\
";

static char help_26 [ ] ="\
tan (X)\n\
\n\
    Computes the tangent of each element of X.  If X is a scalar then the\n\
    result is a scalar.  If X is a matrix then the result is a matrix.\n\
";

static char help_27 [ ] ="\
concat (s, t)\n\
\n\
    Returns the concatenation of s and t, both of which must be strings.\n\
";

static char help_28 [ ] ="\
eval (s)\n\
\n\
    Not yet implemented.\n\
";

static char help_29 [ ] ="\
exit ( )\n\
exit (n)\n\
\n\
    Exits the interpreter with exit code n.  If n is omitted then zero is\n\
    used.\n\
";

static char help_30 [ ] ="\
help ( )\n\
help (s)\n\
\n\
    Requests help on an operation, function, or other topic.  If s is\n\
    omitted then a listing of valid topics is printed.  The argument s\n\
    should be either a string or a function name.\n\
";

static char help_31 [ ] ="\
history ( )\n\
history (n)\n\
\n\
    Prints the command history list.  If n is given then only the last n\n\
    commands are printed.\n\
";

static char help_32 [ ] ="\
include (s)\n\
\n\
    Includes the file named by s.  The file is included in the global\n\
    scope.  The environment variable BURLAP_PATH is used to search for\n\
    the file named by s.\n\
";

static char help_33 [ ] ="\
load (s)\n\
\n\
    Not yet implemented.\n\
";

static char help_34 [ ] ="\
read ( )\n\
\n\
    Reads a line from standard input and returns it as a string.  A null\n\
    value is returned upon end of file.\n\
";

static char help_35 [ ] ="\
reads ( )\n\
\n\
    Reads a string from standard input and returns it.  A null value is\n\
    returned upon end of file.\n\
";

static char help_36 [ ] ="\
save (s)\n\
\n\
    Not yet implemented.\n\
";

static char help_37 [ ] ="\
system (s)\n\
\n\
    Executes the UNIX command named by s.  The command is executed in its\n\
    own subshell.  The return status of the command is returned.\n\
";

static char help_38 [ ] ="\
type (A)\n\
\n\
    Returns a string describing the type of A, which may be of any type.\n\
";

static char help_39 [ ] ="\
write (...)\n\
\n\
    Writes its arguments followed by a newline to standard output.  No\n\
    spaces are automatically written between the arguments, although each\n\
    matrix is written on its own line.\n\
";

static char help_40 [ ] ="\
writes (...)\n\
\n\
    Writes its arguments to standard output.  No spaces are automatically\n\
    written between the arguments, although each matrix is written on its\n\
    own line.\n\
";

static char help_41 [ ] ="\
chol (X)\n\
\n\
    Returns the cholesky decomposition, B, of X, such that B*B' = X.  B\n\
    will be lower triangular.  X must be symmetric and positive definite.\n\
";

static char help_42 [ ] ="\
cols (X)\n\
\n\
    Returns the number of columns of X.  A scalar is defined to have a\n\
    single column.\n\
";

static char help_43 [ ] ="\
compact (X)\n\
\n\
    Returns a compact-storage matrix whose elements are identical to X,\n\
    which must be a symmetric matrix.  The space required by a compact\n\
    matrix is approximately equal to the number of non-zero entries.  The\n\
    compact representation of a scalar is itself.\n\
";

static char help_44 [ ] ="\
det (X)\n\
\n\
    Returns the determinant of X, which must be nonsingular.  The\n\
    determinant of a scalar is itself.\n\
";

static char help_45 [ ] ="\
eig (X)\n\
eig (X, V)\n\
\n\
    Return a column vector containing the eigenvalues of X, which must be\n\
    square.  If X is a scalar then X is returned.  If a variable V is\n\
    specified and X is symmetric then V will contain the matrix of\n\
    eigenvectors on output.  Otherwise, V is ignored.\n\
";

static char help_46 [ ] ="\
eye (m)\n\
eye (m, n)\n\
\n\
    Returns an identity matrix of size (m x n).  If n is omitted then an\n\
    (m x m) matrix is returned.  Both m and n must be scalars.\n\
";

static char help_47 [ ] ="\
inv (X)\n\
\n\
    Returns the inverse of X, or (1/X).  X must be a either a nonsingular\n\
    matrix or a non-zero scalar.\n\
";

static char help_48 [ ] ="\
lu (X)\n\
lu (X, L)\n\
lu (X, L, U)\n\
lu (X, L, U, P)\n\
\n\
    Computes the LU decomposition of X, which must be nonsingular.  The\n\
    return value is row permuted combination of L and U, with the diagonal\n\
    of L not being stored since L is unit lower triangular.  If the\n\
    remaining parameters are variables then they will contain L, U, and/or P\n\
    (the permutation matrix) on output, such that P*L*U=X.\n\
";

static char help_49 [ ] ="\
norm (X)\n\
norm (X, s)\n\
\n\
    Returns the norm of X.  If X is a scalar then s is ignored and the\n\
    absolute value of X is returned.  If X is a vector then s may be one of\n\
    \"1\", \"2\", or \"fro\" indicating that the 1-norm, 2-norm, or frobenius-norm\n\
    (identical to the 2-norm) is to be computed.  The default is to compute\n\
    the 2-norm.  If X is a matrix then s may be either \"1\" or \"fro\"\n\
    indicating that the 1-norm or frobenius-norm is to be computed.  The\n\
    default is to compute the frobenius-norm.\n\
";

static char help_50 [ ] ="\
ones (m)\n\
ones (m, n)\n\
\n\
    Returns a matrix of size (m x n) whose elements are all one.  If n is\n\
    not specified then an (m x m) matrix is returned.  Both m and n must be\n\
    scalars.\n\
";

static char help_51 [ ] ="\
qr (X)\n\
qr (X, Q)\n\
qr (X, Q, R)\n\
\n\
    Computes the QR decomposition of X, which must be overdetermined (tall\n\
    and thin).  The return value is R, which is right triangular, such that\n\
    Q'*X=R.  If a variable Q is specified then it will contain the\n\
    orthogonal matrix of the decomposition on output.\n\
";

static char help_52 [ ] ="\
rand ( )\n\
rand (m)\n\
rand (m, n)\n\
rand (m, n, s)\n\
\n\
    Returns a matrix of size (m x n) with randomly generated elements\n\
    between zero and one.  If n is omitted then an (m x m) matrix is\n\
    returned.  If both m and n are absent then a random scalar is returned.\n\
    If s is specified and is non-zero then it used to seed then random\n\
    number generator.  Both m and n must be scalars.\n\
";

static char help_53 [ ] ="\
rows (X)\n\
\n\
    Returns the number of rows of X.  A scalar is defined to have a\n\
    single row.\n\
";

static char help_54 [ ] ="\
zeros (m)\n\
zeros (m, n)\n\
\n\
    Returns a matrix of size (m x n) whose elements are all zero.  If n is\n\
    not specified then an (m x m) matrix is returned.  Both m and n must be\n\
    scalars.\n\
";

static char help_55 [ ] ="\
X = Y\n\
X := Y\n\
\n\
    Assigns Y to X and returns X.  X must be a variable name, a subsection\n\
    of a matrix (submatrix), or the result of a function call returning a\n\
    global variable.  If X is a submatrix then the dimensions of Y must\n\
    match the dimensions of X.\n\
";

static char help_56 [ ] ="\
m || n\n\
m or n\n\
\n\
    If m evaluates to true (non-zero) then one is returned.  Otherwise, n is\n\
    evaluated and if n is false then zero is returned.  If n is true\n\
    (non-zero) then one is returned.  Both m and n must be scalars.\n\
";

static char help_57 [ ] ="\
m && n\n\
m and n\n\
\n\
    If m evaluates to false (zero) then zero is returned.  Otherwise, n is\n\
    evaluated and if n is false then zero is returned.  If n is true\n\
    (non-zero) then one is returned.  Both m and n must be scalars.\n\
";

static char help_58 [ ] ="\
X == Y\n\
\n\
    Compares each element of X against each element of Y and sets the\n\
    corresponding element of the result to one if X is equal to Y, and zero\n\
    otherwise.  If both X and Y are matrices then their dimensions must\n\
    match.\n\
";

static char help_59 [ ] ="\
X != Y\n\
X <> Y\n\
\n\
    Compares each element of X against each element of Y and sets the\n\
    corresponding element of the result to one if X is not equal to Y, and\n\
    zero otherwise.  If both X and Y are matrices then their dimensions must\n\
    match.\n\
";

static char help_60 [ ] ="\
X < Y\n\
\n\
    Compares each element of X against each element of Y and sets the\n\
    corresponding element of the result to one if X is less than to Y, and\n\
    zero otherwise.  If both X and Y are matrices then their dimensions must\n\
    match.\n\
";

static char help_61 [ ] ="\
X > Y\n\
\n\
    Compares each element of X against each element of Y and sets the\n\
    corresponding element of the result to one if X is greater than Y, and\n\
    zero otherwise.  If both X and Y are matrices then their dimensions must\n\
    match.\n\
";

static char help_62 [ ] ="\
X <= Y\n\
\n\
    Compares each element of X against each element of Y and sets the\n\
    corresponding element of the result to one if X is less than or equal to\n\
    Y, and zero otherwise.  If both X and Y are matrices then their\n\
    dimensions must match.\n\
";

static char help_63 [ ] ="\
X >= Y\n\
\n\
    Compares each element of X against each element of Y and sets the\n\
    corresponding element of the result to one if X is greater than or equal\n\
    to Y, and zero otherwise.  If both X and Y are matrices then their\n\
    dimensions must match.\n\
";

static char help_64 [ ] ="\
m : n\n\
m : k : n\n\
\n\
    Returns a row vector starting with values m through n.  If k is given\n\
    then it is used as the increment between successive values.  Otherwise,\n\
    the increment is one.\n\
";

static char help_65 [ ] ="\
+ X\n\
X + Y\n\
\n\
    In the unary form, returns X.  In the binary form, returns the sum of X\n\
    and Y.  If X and Y are both scalars then scalar addition is performed.\n\
    If one is a scalar and the other is a matrix then the scalar value is\n\
    added to each element of the matrix.  If both are matrices then matrix\n\
    addition is performed, and the dimensions of each must agree.\n\
";

static char help_66 [ ] ="\
- X\n\
X - Y\n\
\n\
    In the unary form, returns the negative of X.  In the binary form,\n\
    returns the difference of X and Y.  If X and Y are both scalars then\n\
    subtraction is performed.  If X is a matrix and Y is a scalar then Y is\n\
    subtracted from each element of X.  If X is a scalar and Y is a matrix\n\
    then each element of X is subtracted from Y.  If both are matrices then\n\
    matrix subtraction is performed, and the dimensions of each must agree.\n\
";

static char help_67 [ ] ="\
X * Y\n\
\n\
    Returns the product of X and Y.  If X and Y are both scalars then scalar\n\
    multiplication is performed.  If one is a scalar and the other is a\n\
    matrix then the matrix is scaled by the scalar value.  If both are\n\
    matrices then the matrix multiplication is performed, and the inner\n\
    dimensions must agree.\n\
";

static char help_68 [ ] ="\
X \\ Y\n\
\n\
    Returns the \"left division\" of X and Y, or (1/X) * Y.  If X and Y are\n\
    matrices then an LU decomposition is used to compute the \"inverse\" of X.\n\
    If X is a matrix but Y is a scalar then the true inverse of X is scaled\n\
    by Y.\n\
";

static char help_69 [ ] ="\
X / Y\n\
\n\
    Returns the \"right division\" of X and Y, or X * (1/Y).  If X and Y are\n\
    matrices then an LU decomposition (along with transposition) is used to\n\
    compute the \"inverse\" of Y.  If X is a scalar but Y is a matrix then the\n\
    true inverse of Y is scaled by X.\n\
";

static char help_70 [ ] ="\
X % Y\n\
fmod (X, Y)\n\
\n\
    Returns the modulo of X and Y.  If X and Y are both scalars then the\n\
    scalar remainder is computed.  If X is a matrix and Y is a scalar then\n\
    each element of X is computed modulo Y.  If X is a scalar and Y is a\n\
    matrix then X is computed modulo each element of Y.  If both are\n\
    matrices then each element of X is computed modulo each element of Y,\n\
    and the dimensions of each must agree.\n\
";

static char help_71 [ ] ="\
m ^ n\n\
m ** n\n\
pow (m, n)\n\
\n\
    Returns m raised to the power n, where m is non-negative or n is an\n\
    integer value.  Both m and n must be scalars.\n\
";

static char help_72 [ ] ="\
X '\n\
\n\
    Returns the transpose of X.  The transpose of a scalar is itself.\n\
";

static char help_73 [ ] ="\
! X\n\
not X\n\
\n\
    Returns the logical negation of X.  If X is a matrix then each element\n\
    of X is negated.  The logical negation of zero is one and the logical\n\
    negation of a non-zero value is zero.\n\
";

static char help_74 [ ] ="\
( X )\n\
X ( ... )\n\
\n\
    In the first form, which may be used for enforcing precedence, the\n\
    result is X.  In the second form, if X is a matrix then the result is a\n\
    subsection of the matrix (submatrix).  The number of indices must be\n\
    appropriate.  If an index is a vector then it designates a series of\n\
    rows or columns and must be contiguous.  The special index : may be used\n\
    to designate an entire row or column.  If X is an array then the return\n\
    value is the result of indexing the array.  Otherwise, X is evaluated as\n\
    a function and the remaining expressions are passed as arguments.  The\n\
    result is the return value of the function call.\n\
";

static char help_75 [ ] ="\
[ ... ; ... ; ... ]\n\
\n\
    Returns a matrix.  A semicolon (or return) separates one row for the\n\
    next.  Matrix elements on the same row are separated with commas.  The\n\
    matrix elements may be matrices or scalars, but all elements on the same\n\
    row must have the same number of rows.  Each row must also have the same\n\
    number of columns.\n\
";

static char help_76 [ ] ="\
X . id\n\
\n\
    Returns the field id of structure X.  This operator is used to access\n\
    members of the FElt data structures.\n\
";

static char help_77 [ ] ="\
any? (X)\n\
\n\
    Returns true (one) if any element of X is non-zero.  Otherwise, returns\n\
    false (zero).  X must be a matrix or a scalar.\n\
";

static char help_78 [ ] ="\
compact? (A)\n\
\n\
    Returns true (one) if A is a compact-storage matrix.  Otherwise, returns\n\
    false (zero).\n\
";

static char help_79 [ ] ="\
every? (X)\n\
\n\
    Returns true (one) if every element of X is non-zero.  Otherwise, returns\n\
    false (zero).  X must be a matrix or a scalar.\n\
";

static char help_80 [ ] ="\
matrix? (A)\n\
\n\
    Returns true (one) if A is a matrix (and not a scalar).  Otherwise,\n\
    returns false (zero).\n\
";

static char help_81 [ ] ="\
null? (A)\n\
\n\
    Returns true (one) if A is null (has not been assigned a value).\n\
    Otherwise, returns false (zero).\n\
";

static char help_82 [ ] ="\
scalar? (A)\n\
\n\
    Returns true (one) if A is a scalar.  Otherwise, returns false (zero).\n\
";

static char help_83 [ ] ="\
symmetric? (X)\n\
\n\
    Returns true (one) if any X is a symmetric matrix or a scalar.  Otherwise,\n\
    false (zero).  X must be a matrix or a scalar.\n\
";

static struct {
    int   type;
    const char *key;
    char *message;
} help [ ] = {
    {0, "!", help_73},
    {0, "!=", help_59},
    {0, "%", help_70},
    {0, "&&", help_57},
    {0, "'", help_72},
    {0, "(", help_74},
    {0, "()", help_74},
    {0, ")", help_74},
    {0, "*", help_67},
    {0, "**", help_71},
    {0, "+", help_65},
    {0, "-", help_66},
    {0, ".", help_76},
    {0, "/", help_69},
    {0, ":", help_64},
    {0, ":=", help_55},
    {0, "<", help_60},
    {0, "<=", help_62},
    {0, "<>", help_59},
    {0, "=", help_55},
    {0, "==", help_58},
    {0, ">", help_61},
    {0, ">=", help_63},
    {0, "[", help_75},
    {0, "[]", help_75},
    {0, "\\", help_68},
    {0, "]", help_75},
    {0, "^", help_71},
    {0, "and", help_57},
    {0, "not", help_73},
    {0, "or", help_56},
    {0, "||", help_56},
    {1, "abs", help_16},
    {1, "ceil", help_17},
    {1, "chol", help_41},
    {1, "cols", help_42},
    {1, "compact", help_43},
    {1, "cos", help_18},
    {1, "det", help_44},
    {1, "eig", help_45},
    {1, "exp", help_19},
    {1, "eye", help_46},
    {1, "fabs", help_16},
    {1, "floor", help_20},
    {1, "fmod", help_70},
    {1, "hypot", help_21},
    {1, "inv", help_47},
    {1, "log", help_22},
    {1, "log10", help_23},
    {1, "lu", help_48},
    {1, "norm", help_49},
    {1, "ones", help_50},
    {1, "pow", help_71},
    {1, "qr", help_51},
    {1, "rand", help_52},
    {1, "rows", help_53},
    {1, "sin", help_24},
    {1, "sqrt", help_25},
    {1, "tan", help_26},
    {1, "zeroes", help_54},
    {1, "zeros", help_54},
    {2, "any?", help_77},
    {2, "compact?", help_78},
    {2, "every?", help_79},
    {2, "matrix?", help_80},
    {2, "null?", help_81},
    {2, "scalar?", help_82},
    {2, "symmetric?", help_83},
    {3, "concat", help_27},
    {3, "eval", help_28},
    {3, "exit", help_29},
    {3, "help", help_30},
    {3, "history", help_31},
    {3, "include", help_32},
    {3, "load", help_33},
    {3, "read", help_34},
    {3, "reads", help_35},
    {3, "save", help_36},
    {3, "system", help_37},
    {3, "type", help_38},
    {3, "write", help_39},
    {3, "writes", help_40},
    {4, "assemble", help_0},
    {4, "clear_nodes", help_1},
    {4, "compute_modes", help_2},
    {4, "compute_stresses", help_3},
    {4, "construct_forces", help_4},
    {4, "find_dofs", help_5},
    {4, "global_dof", help_6},
    {4, "integrate_hyperbolic", help_7},
    {4, "integrate_parabolic", help_8},
    {4, "local_dof", help_9},
    {4, "remove_constrained", help_10},
    {4, "renumber_nodes", help_11},
    {4, "restore_numbers", help_12},
    {4, "set_up", help_13},
    {4, "solve_displacements", help_14},
    {4, "zero_constrained", help_15},
};

static struct {
    const char *title;
    int   across;
    int   width;
} help_topics [ ] = {
    {"operators", 10, 7},
    {"mathematical functions", 7, 10},
    {"predicate functions", 4, 14},
    {"miscellaneous functions", 7, 10},
    {"finite element functions", 3, 22},
};
