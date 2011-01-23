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
 * File:	util.h							*
 *									*
 * Description:	This file contains the function declarations for the	*
 *		miscellaneous utility functions.			*
 ************************************************************************/

# ifndef _UTIL_H
# define _UTIL_H

#ifdef __cplusplus
extern "C" {
#endif 

/*----------------------------------------------------------------------*/

XtArgVal StringToLayout (Widget widget, String string);

void CenterOnWidget (Widget shell, Widget center, Boolean force);

void CenterOnScreen (Widget shell, Boolean force);

void AddDeleteWindowProtocol (Widget shell, String action);

void WarpToCenter (Widget w);

void ListAddCursorTranslations (Widget viewport);

void ListAddCursorAccelerators (Widget viewport, Widget w);

void SetTextString (Widget w, const char *value);

String GetTextString (Widget w);

Cardinal GetTextWidth (XFontStruct *font, String text, Cardinal length);

void SetLabelString (Widget w, const char *value);

String GetLabelString (Widget w);

void AddAutoRepeatAction (XtAppContext app_context);

Widget CreateHelpButton (Widget parent, String name);

void UpdateHelpMessage (Widget button, String message, Dimension width);

/*----------------------------------------------------------------------*/

#ifdef __cplusplus
}
#endif 

# endif /* _UTIL_H */
