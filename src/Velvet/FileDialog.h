/*
    This file is part of the FElt finite element analysis package.
    Copyright (C) 1993-1997 Jason I. Gobat and Darren C. Atkinson

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
 * File:	FileDialog.h						*
 *									*
 * Description:	This file contains the public function and type		*
 *		declarations for the file file dialog box.		*
 ************************************************************************/

# ifndef _FileDialog_h
# define _FileDialog_h

typedef struct file_dialog *FileDialog;


extern FileDialog FileDialogCreate (
# if NeedFunctionPrototypes
    Widget		/* parent  */,
    String		/* name    */,
    String *		/* toggles */
# endif
);


extern void FileDialogSelect (
# if NeedFunctionPrototypes
    FileDialog		/* file_dialog */,
    String		/* title       */,
    String 		/* label       */,
    String		/* suggestion  */,
    String *		/* answer      */,
    String *		/* toggled     */
# endif
);


extern void FileDialogPopup (
# if NeedFunctionPrototypes
    FileDialog		/* file_dialog */,
    String		/* title       */,
    String		/* label       */,
    String		/* suggestion  */,
    XtCallbackProc	/* callback    */,
    XtPointer		/* client_data */
# endif
);


extern String FileDialogToggle (
# if NeedFunctionPrototypes
    FileDialog		/* file_dialog */
# endif
);


extern void FileDialogSetToggles (
# if NeedFunctionPrototypes
    FileDialog		/* file_dialog */,
    String		/* label_1     */,
    String		/* label_2     */
# endif
);


extern void FileDialogPopdown (
# if NeedFunctionPrototypes
    FileDialog		/* file_dialog */
# endif
);

# endif /* _FileDialog_h */
