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
 * File:	vector.c						*
 *									*
 * Description:	This file contains the function definitions for the	*
 *		vector virtual machine instructions.			*
 ************************************************************************/

# include <math.h>
# include "debug.h"
# include "error.h"
# include "coerce.h"
# include "vector.h"
# include "execute.h"


/************************************************************************
 * Function:	mtx_op							*
 *									*
 * Description:	Pushes a matrix descriptor on the stack.  The		*
 *		descriptor is constructed from the rows contained on	*
 *		the stack.  Each row is separated by a row marker.  If	*
 *		the resulting matrix consists of only a single scalar	*
 *		then the result will be a double scalar.  An attempt is	*
 *		made to coerce all elements to double values.		*
 ************************************************************************/

int mtx_op ( )
{
    int		i;
    int		j;
    int		rs;
    int		width;
    int		height;
    int	 	nelts;
    int		nmarks;
    int		ncols;
    int		nrows;
    int		curr_col;
    int		curr_row;
    int		offset;
    int		depth;
    Matrix	m;
    Matrix	n;
    descriptor *d;
    descriptor *e;
    descriptor  t;


    nelts = 0;
    nrows = 0;
    ncols = 0;
    offset = 0;
    height = 0;
    nmarks = fetch (pc ++).ival;

    for (i = 0; i < nmarks; i ++) {
	d = ntop (offset ++);
	rs = D_Row (d);
	width = 0;
	height = 0;

	for (j = 0; j < rs; j ++) {
	    d = ntop (offset ++);
	    d = CoerceData (deref (d), T_Double);

	    switch (D_Type (d)) {
	    case T_Double:
		width += 1;
		if (height == 0)
		    height = 1;
		else if (height != 1) {
		    rterror ("inconsistent number of rows");
		    return 1;
		}
		break;


	    case T_Matrix:
		width += Mcols (D_Matrix (d));
		if (height == 0)
		    height = Mrows (D_Matrix (d));
		else if (height != Mrows (D_Matrix (d))) {
		    rterror ("inconsistent number of rows");
		    return 1;
		}
		break;


	    default:
		TypeError ("in matrix constructor", d, NULL, NULL, F_False);
		return 1;
	    }
	}

	nrows += height;

	if (ncols == 0)
	    ncols = width;
	else if (width != ncols) {
	    rterror ("inconsistent number of columns");
	    return 1;
	}
    }

    d = &t;
    depth = offset;

    if (nrows == 1 && ncols == 1) {
	CreateData (d, NULL, NULL, T_Double);
	e = ntop (-- offset);
	e = CoerceData (deref (e), T_Double);
	*D_Double (d) = *D_Double (e);

    } else {
	CreateData (d, NULL, NULL, T_Matrix, nrows, ncols);
	m = D_Matrix (d);
	curr_row = 1;

	while (curr_row <= nrows) {
	    curr_col = 1;
	    while (curr_col <= ncols) {
		e = ntop (-- offset);
		e = CoerceData (deref (e), T_Double);

		switch (D_Type (e)) {
		case T_Double:
		    sdata (m, curr_row, curr_col ++) = *D_Double (e);
		    height = 1;
		    break;

		case T_Matrix:
		    n = D_Matrix (e);
		    for (i = 1; i <= Mrows (n); i ++)
			for (j = 1; j <= Mcols (n); j ++)
			    sdata (m, curr_row + i - 1, curr_col + j - 1) =
				mdata (n, i, j);

		    curr_col += Mcols (n);
		    height = Mrows (n);
		    break;


		default:
		    height = 0;
		    break;
		}
	    }

	    curr_row += height;
	}
    }

    for (i = 0; i < depth; i ++)
	RecycleData (pop ( ));

    e = push ( );
    *e = *d;

    d_printf ("mtx ans =\n");
    d_PrintData (d);
    return 0;
}


/************************************************************************
 * Function:	range_op						*
 *									*
 * Description:	Pushes a matrix descriptor on the stack.  The		*
 *		descriptor is constructed using the starting value,	*
 *		ending value, and increment contained on the stack.	*
 *		The resulting matrix will be a row vector.  If the	*
 *		increment is not presented then it is assumed to be	*
 *		one.  If no operands are provided then a row marker is	*
 *		placed on the stack.  The marker is an indicator to	*
 *		matrix indexing that the entire row or column should be	*
 *		used as an index.  If the range is improper then the	*
 *		result will be a null descriptor.			*
 ************************************************************************/

int range_op ( )
{
    Matrix	a;
    descriptor *by;
    descriptor *to;
    descriptor *from;
    descriptor *result;
    descriptor	temp;
    double	start;
    double	end;
    double	inc;
    int		type_error;
    int		status;
    int		n;
    unsigned	i;
    unsigned	num;


    status = 0;
    type_error = F_False;


    if ((n = fetch (pc ++).ival) == 0) {
	result = push ( );
	CreateData (result, NULL, NULL, T_Row);

    } else {
	to = pop ( );
	by = n == 3 ? pop ( ) : NULL;
	result = top ( );
	temp = *result;
	from = &temp;


	from = CoerceData (deref (from), T_Double);
	to = CoerceData (deref (to), T_Double);
	if (by)
	    by = CoerceData (deref (by), T_Double);


	if (D_Type (from) != T_Double) {
	    TypeError (":", from, by, to, F_False);
	    type_error = F_True;

	} else if (D_Type (to) != T_Double) {
	    TypeError (":", from, by, to, F_False);
	    type_error = F_True;

	} else if (by && D_Type (by) != T_Double) {
	    TypeError (":", from, by, to, F_False);
	    type_error = F_True;

	} else {
	    start = *D_Double (from);
	    end = *D_Double (to);
	    inc = by ? *D_Double (by) : 1;

	    if (start <= end && inc > 0) {
		num = (end - start) / inc + 1;
		CreateData (result, NULL, NULL, T_Matrix, 1, num);
		a = D_Matrix (result);

		for (i = 1; i <= num; i ++) {
		    sdata (a, 1, i) = start;
		    start += inc;
		}


	    } else if (start >= end && inc < 0) {
		num = (end - start) / inc + 1;
		CreateData (result, NULL, NULL, T_Matrix, 1, num);
		a = D_Matrix (result);

		for (i = 1; i <= num; i ++) {
		    sdata (a, 1, i) = start;
		    start += inc;
		}


	    } else
		CreateData (result, NULL, NULL, T_Null);
	}


	RecycleData (to);
	RecycleData (by);
	RecycleData (from);
    }


    d_printf ("range ans =\n");
    d_PrintData (result);

    return type_error == F_True || status != 0;
}


/************************************************************************
 * Function:	row_op							*
 *									*
 * Description:	Pushes a row mark descriptor on the stack.		*
 ************************************************************************/

int row_op ( )
{
    descriptor *d;


    d = push ( );
    D_Type (d) = T_Row;
    D_Row  (d) = fetch (pc ++).ival;
    return 0;
}
