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
 * File:	descriptor.h						*
 *									*
 * Description:	This file contains the public function, type, and	*
 *		constant declarations for descriptors.  A descriptor	*
 *		is an object containing type information and a pointer	*
 *		to the actual data.  Each type needs to have a type	*
 *		constant defined here, a corresponding entry in the	*
 *		array of type names, and a construction and destruction	*
 *		function.						*
 ************************************************************************/

# ifndef _DESCRIPTOR_H
# define _DESCRIPTOR_H
# include "array.h"			/* Array type definition    */
# include "matrix.h"			/* Matrix type definition   */
# include "function.h"			/* Function type definition */

#ifdef __cplusplus
extern "C" {
#endif 

/*----------------------------------------------------------------------*/

# define F_False	0
# define F_True		1

# define T_Null		0
# define T_Variable	1
# define T_Function	2
# define T_Intrinsic	3
# define T_String	4
# define T_Double	5
# define T_Int		6
# define T_Byte		7
# define T_Array	8
# define T_Row		9
# define T_Matrix	10
# define T_MatrixPtr	11
# define T_Analysis	12
# define T_Constraint	13
# define T_Definition	14
# define T_Element	15
# define T_Force	16
# define T_Load		17
# define T_Material	18
# define T_Node		19
# define T_Pair		20
# define T_Problem	21
# define T_Stress	22
# define T_External	23

# define D_NumTypes	(T_External + 1)

# define D_Type(d)	 ((d) -> type)
# define D_Temp(d)	 ((d) -> temp)
# define D_Trapped(d)	 ((d) -> trapped)
# define D_Union(d)	 ((d) -> u)
# define D_Pointer(d)	 ((d) -> u.ptr)
# define D_Variable(d)	 ((d) -> u.var)
# define D_Function(d)	 ((d) -> u.func)
# define D_Intrinsic(d)	 ((d) -> u.intr)
# define D_String(d)	 ((d) -> u.sval)
# define D_Double(d)	 ((d) -> u.dval)
# define D_Int(d)	 ((d) -> u.ival)
# define D_Byte(d)	 ((d) -> u.bval)
# define D_Array(d)	 ((d) -> u.array)
# define D_Row(d)	 ((d) -> u.row)
# define D_Matrix(d)	 ((d) -> u.mtx)
# define D_MatrixPtr(d)	 ((Matrix *) (d) -> u.ptr)
# define D_Analysis(d)	 ((Analysis *) (d) -> u.ptr)
# define D_Constraint(d) ((Constraint *) (d) -> u.ptr)
# define D_Definition(d) ((Definition *) (d) -> u.ptr)
# define D_Element(d)	 ((Element *) ((d) -> u.ptr))
# define D_Force(d)	 ((Force *) (d) -> u.ptr)
# define D_Load(d)	 ((Distributed *) (d) -> u.ptr)
# define D_Material(d)	 ((Material *) (d) -> u.ptr)
# define D_Node(d)	 ((Node *) (d) -> u.ptr)
# define D_Pair(d)	 ((Pair *) (d) -> u.ptr)
# define D_Problem(d)	 ((Problem *) (d) -> u.ptr)
# define D_Stress(d)	 ((Stress *) (d) -> u.ptr)
# define D_External(d)	 ((int (**) ( )) (d) -> u.ptr)
# define D_TypeName(d)	 (type_names [D_Type (d)])
# define D_Direct(d)	 (D_Type (d) == T_Analysis || \
			  D_Type (d) == T_Problem || D_Type (d) == T_Pair)
# define D_Writable(d)	 (D_Temp (d) && \
			 (D_Type (d) != T_Matrix || IsFull (D_Matrix (d))))

typedef struct descriptor descriptor;

struct descriptor {
    short	    type;		/* type code		     */
    char	    temp;		/* temporary data flag	     */
    char	    trapped;		/* trapped variable flag     */
    union {
	int	    row;		/* number of elements in row */
	descriptor *var;		/* data for T_Variable type  */
	Function    func;		/* data for T_Function type  */
	int	    intr;		/* data for T_Intrinsic type */
	char	  **sval;		/* data for T_String type    */
	double	   *dval;		/* data for T_Double type    */
	int	   *ival;		/* data for T_Integer type   */
	char	   *bval;		/* data for T_Byte type      */
	Array	    array;		/* data for T_Array type     */
	Matrix	    mtx;		/* data for T_Matrix type    */
	void	   *ptr;		/* data for all other types  */
    } u;
};


extern const char *type_names [ ];

void CreateData (descriptor *d, descriptor *a, descriptor *b, int type, ...);

int AssignData (descriptor *dest, descriptor **srcp);

int AssignObject (descriptor *dest, int type, int temp, void *ptr);

void RecycleData (descriptor *d);

void FreeData (descriptor *d);

void PrintData (descriptor *d);

/*----------------------------------------------------------------------*/

#ifdef __cplusplus
}
#endif 

# endif /* _DESCRIPTOR_H */
