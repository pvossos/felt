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
 * File:	FileDialog.h						*
 *									*
 * Description:	This file contains the public function and type		*
 *		declarations for the file file dialog box.		*
 ************************************************************************/

# ifndef _FileDialog_h
# define _FileDialog_h

#ifdef __cplusplus
extern "C" {
#endif 

/*----------------------------------------------------------------------*/

typedef struct file_dialog *FileDialog;

FileDialog FileDialogCreate (Widget parent, String name, char **toggle_labels);

void FileDialogSelect (FileDialog filed, String title, String label, String suggestion, String *answer, String *toggle);

void FileDialogPopup (FileDialog filed, String title, String label, String suggestion, XtCallbackProc callback, XtPointer client_data);

String FileDialogToggle (FileDialog filed);

void FileDialogSetToggles (FileDialog filed, String label1, String label2);

void FileDialogPopdown (FileDialog filed);

/*----------------------------------------------------------------------*/

#ifdef __cplusplus
}
#endif 

# endif /* _FileDialog_h */
