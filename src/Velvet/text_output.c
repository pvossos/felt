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

/*****************************************************************************
 *
 * File:	text_output.c
 *
 ****************************************************************************/

# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Xaw/AsciiText.h>
# include <X11/Xaw/Form.h>
# include <X11/Xaw/Label.h>
# include <X11/Xaw/Dialog.h>
# include <X11/Xaw/Viewport.h>
# include <X11/Shell.h>
# include "util.h"
# include "OutputDialog.h"
# include "fe.h"
# include "FileDialog.h"
# include "Tree.h"
# include "globals.h"
# include "procedures.h"

extern FileDialog	filed;
 
void OutputWindowPopup ( )
{
   OutputDialogPopup (output_dialog, "FElt Output","dismiss",
                      OutputButtonActions, NULL);
}

void OutputButtonActions (w, client_data, call_data)
   Widget 	w;
   XtPointer	client_data,
		call_data;
{
   String      selected = (String) call_data;
   String      ans;
   Widget      source;

   if (selected == NULL || strcmp (selected, "dismiss") == 0)
      OutputDialogPopdown (output_dialog);

   else if (strcmp (selected, "save")==0) {
      CenterOnScreen (XtNameToWidget (toplevel, "fileDialog"), True);
      WarpToCenter (XtNameToWidget (toplevel, "fileDialog"));
      FileDialogSelect (filed, "Save Output","Save as:", "", &ans, NULL);
      if (ans != NULL) {
         source = XawTextGetSource 
            (XtNameToWidget(OutputDialogShell(output_dialog),"layout.text"));
         XawAsciiSaveAsFile (source, ans);
      }
   }
}
