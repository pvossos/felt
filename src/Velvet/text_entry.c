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

/**************************************************************************
* 
* File:		text_entry.c
*
* Description:	contains various text entry utility functions.
*
**************************************************************************/

# include <stdio.h>
# include <string.h>
# include <stdlib.h>
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Xaw/Simple.h>
# include <X11/Shell.h>
# include "Canvas.h"
# include "fe.h"
# include "text_entry.h"
# include "Drawing.h"
# include "Tree.h"
# include "globals.h"
# include "procedures.h"

char *GetTextCoordinates (float *rx, float *ry, float *rz)
{
   String	result;
   Arg		arglist [2];
   Cardinal	count;
   float	x,y,z;
   int		status;
   int		desired;
   static char	buffer [256];

   count = 0; 
   XtSetArg (arglist [count], XtNstring, &result); count++;
   XtGetValues (entry, arglist, count);

   if (ry == NULL && rz == NULL) {
      desired = 1;
      status = sscanf (result, "%f %s",&x,buffer);
   }
   else if (rz == NULL) {
      desired = 2;
      status = sscanf (result, "%f%*[, ]%f %s",&x,&y,buffer);
   }
   else {
      desired = 3;
      status = sscanf (result, "%f%*[, ]%f%*[, ]%f %s",&x,&y,&z,buffer);
   }

   count = 0; 
   XtSetArg (arglist [count], XtNstring, ""); count++;
   XtSetValues (entry, arglist, count);
   
   if (status == desired) {
      *rx = x;
      if (ry != NULL)
         *ry = y;
      if (rz != NULL)
         *rz = z;

      return NULL;
   } 
   else {
      XBell (XtDisplay (toplevel), 20); 
      return buffer;
   }
}

char *GetTextNumber (unsigned int *number)
{
   String	result;
   static char  buffer [256];
   Arg		arglist [2];
   Cardinal	count;
   char		*ptr;
   unsigned	success;
   long 	num;

   count = 0; 
   XtSetArg (arglist [count], XtNstring, &result); count++;
   XtGetValues (entry, arglist, count);

   success = 1;

   num = strtol (result, &ptr, 10);
   if (*ptr == 0) 
      *number = num;
   else {
      success = 0;
      (void) strcpy (buffer, result);
   }

   count = 0; 
   XtSetArg (arglist [count], XtNstring, ""); count++;
   XtSetValues (entry, arglist, count);
   
   if (success) 
      return NULL;
   else {
      XBell (XtDisplay (toplevel), 20); 
      return buffer;
   }
}

String GetText (void)
{
   String	result;
   String	copy;
   Arg		arglist [1];

   XtSetArg (arglist [0], XtNstring, &result); 
   XtGetValues (entry, arglist, 1);

   if (strcmp (result, "") == 0)
      copy = NULL;
   else
      copy = XtNewString (result);

   XtSetArg (arglist [0], XtNstring, ""); 
   XtSetValues (entry, arglist, 1);

   return copy;
}
