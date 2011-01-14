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

/************************************************************************
 * File:	exectab.h						*
 *									*
 * Description:	This file contains the table of functions that execute	*
 *		each virtual machine instruction.  Note that this table	*
 *		must be kept in correspondence with the opcodes listed	*
 *		in opcodes.h!						*
 ************************************************************************/

# ifndef _EXECTAB_H
# define _EXECTAB_H
# include "apply.h"
# include "field.h"
# include "vector.h"
# include "control.h"
# include "literal.h"
# include "location.h"
# include "arithmetic.h"
# include "assignment.h"
# include "relational.h"

#ifdef __cplusplus
extern "C" {
#endif 

/*----------------------------------------------------------------------*/

static int (*(exectab [ ])) (void) = {
    add_op,	/* arithmetic.c	*/
    apply_op,	/* apply.c	*/
    arg_op,	/* literal.c	*/
    asgn_op,	/* assignment.c	*/
    bkslv_op,	/* arithmetic.c	*/
    copy_op,	/* assignment.c	*/
    dbl_op,	/* literal.c	*/
    div_op,	/* arithmetic.c	*/
    eq_op,	/* relational.c	*/
    fail_op,	/* control.c	*/
    field_op,	/* field.c	*/
    file_op,	/* location.c	*/
    gen_op,	/* control.c	*/
    ge_op,	/* relational.c	*/
    glbl_op,	/* literal.c	*/
    gt_op,	/* relational.c	*/
    halt_op,	/* control.c	*/
    jmp_op,	/* control.c	*/
    jnz_op,	/* control.c	*/
    jz_op,	/* control.c	*/
    le_op,	/* relational.c	*/
    line_op,	/* location.c	*/
    local_op,	/* literal.c	*/
    lt_op,	/* relational.c	*/
    mod_op,	/* arithmetic.c	*/
    mtx_op,	/* vector.c	*/
    mul_op,	/* arithmetic.c	*/
    ne_op,	/* relational.c	*/
    neg_op,	/* arithmetic.c	*/
    not_op,	/* arithmetic.c	*/
    null_op,	/* literal.c	*/
    plus_op,	/* arithmetic.c	*/
    pop_op,	/* assignment.c	*/
    pow_op,	/* arithmetic.c	*/
    range_op,	/* vector.c	*/
    row_op,	/* vector.c	*/
    rtn_op,	/* apply.c	*/
    str_op,	/* literal.c	*/
    sub_op,	/* arithmetic.c	*/
    test_op,	/* assignment.c	*/
    trans_op,	/* arithmetic.c	*/
};

/*----------------------------------------------------------------------*/

#ifdef __cplusplus
}
#endif 

# endif /* _EXECTAB_H */
