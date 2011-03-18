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
 * File:	load.c							*
 *									*
 * Description:	This file contains the public and private function and	*
 *		type definitions for the load dialog box.		*
 ************************************************************************/

# include <algorithm>
# include <stdio.h>
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Shell.h>
# include <X11/Xaw/AsciiText.h>
# include <X11/Xaw/Command.h>
# include <X11/Xaw/Label.h>
# include <X11/Xaw/List.h>
# include <X11/Xaw/Toggle.h>
# include <X11/Xaw/Viewport.h>
# include "Layout.h"
# include "Load.h"
# include "TabGroup.h"
# include "util.h"
# include "fe.h"
# include "allocate.h"
# include "setaux.hpp"

# ifndef X_NOT_STDC_ENV
# include <stdlib.h>
# else
extern long   strtol ( );
extern int    atoi   ( );
# endif


struct load_dialog {
    Widget         shell;		/* topLevelShell  <specified>	  */
    Widget         layout;		/*	Layout  layout		  */
    Widget         name;		/*	     AsciiText  name	  */
    Widget	   localX;		/*	     Toggle     LocalX	  */
    Widget	   localY;		/* 	     Toggle	LocalY	  */
    Widget	   localZ;		/*	     Toggle	LocalZ	  */
    Widget	   globalX;		/*	     Toggle	GlobalX	  */
    Widget	   globalY;		/*	     Toggle     GlobalY   */
    Widget	   globalZ;		/*	     Toggle     GlobalZ   */
    Widget	   parallel;		/*	     Toggle	parallel  */
    Widget	   perpendicular;	/*	     Toggle	perpendic */
    Widget	   radial;	
    Widget	   axial;	
    Widget	   nodenum[4];		/*	     AsciiText  nodenum   */
    Widget	   magnitude[4];	/*	     AsciiText  magnitude */
    Widget         viewport;		/*	     Viewport  viewport	  */
    Widget         list;		/*		  List  list	  */
    Widget         help;		/*	     MenuButton  help	  */
    Widget         accept;		/*	     Command  accept	  */
    Widget         dismiss;		/*	     Command  dismiss	  */
    Widget         nuke;		/*	     Command  delete	  */
    Widget         nu;			/*	     Command  new	  */
    Widget         copy;		/*	     Command  copy	  */
    XtCallbackProc callback;
    XtPointer	   closure;
    String        *loads;
    Distributed    active;
    Boolean        new_copy;
    Problem::DistributedSet *tree;
};

static const char* labels [ ] = {
    "Name:", "LocalX", "LocalY", "LocalZ", "GlobalX", "GlobalY", 
    "GlobalZ", "perpend", "parallel", "radial", "axial", "Node", "Magnitude"
};

static const char* names [ ] = {
     "nameLabel","localX_label","localY_label","localZ_label","globalX_label",
     "globalY_label", "globalZ_label", "perpendicular_label",
     "parallel_label", "radial_label", "axial_label", "node_label", "magnitude_label"
};

static LoadDialog  dialog;
static Cardinal	   num_loads;
static int	   list_index;
static unsigned	   Button = 1,
		   Forced = 2;


/* Resources */

static Pixel highlight;

static String dummy_list [ ] = {
    NULL
};

static String layout_string =
"vertical { \
     horizontal { 4 \
 	vertical { 4 \
 	    horizontal { \
 		nameLabel \
 		4 \
 		name <+inf -100% *> \
 	    } \
 	    4 \
 	    viewport <+inf * +inf> \
 	    4 \
 	} \
 	4 \
	separator1 <* +inf -100%> \
	4 \
        vertical { 4 height name 4 \
            horizontal { \
 	        vertical { \
                    ((height localX - height localX_label) / 2) \
                    localX_label \
                    ((height localX - height localX_label) / 2) \
                    4 \
                    ((height localY - height localY_label) / 2) \
                    localY_label \
                    ((height localY - height localY_label) / 2) \
                    4 \
                    ((height localZ - height localZ_label) / 2) \
                    localZ_label \
                    ((height localZ - height localZ_label) / 2) \
                    4 \
                    ((height perpendicular - height perpendicular_label) / 2) \
                    perpendicular_label \
                    ((height perpendicular - height perpendicular_label) / 2) \
                    4 \
                    ((height axial - height axial_label) / 2) \
                    axial_label \
                    ((height axial - height axial_label) / 2) \
                    4 \
             	} \
     	        4 \
                vertical { \
                    localX \
                    4 \
                    localY \
                    4 \
                    localZ \
                    4 \
                    perpendicular \
 	            4 \
                    axial \
 	            4 \
 	        } \
 	        4 \
                vertical { \
                    ((height globalX - height globalX_label) / 2) \
                    globalX_label \
                    ((height globalX - height globalX_label) / 2) \
                    4 \
                    ((height globalY - height globalY_label) / 2) \
                    globalY_label \
                    ((height globalY - height globalY_label) / 2) \
                    4 \
                    ((height globalZ - height globalZ_label) / 2) \
                    globalZ_label \
                    ((height globalZ - height globalZ_label) / 2) \
                    4 \
                    ((height parallel - height parallel_label) / 2) \
                    parallel_label \
                    ((height parallel - height parallel_label) / 2) \
                    4 \
                    ((height radial - height radial_label) / 2) \
                    radial_label \
                    ((height radial - height radial_label) / 2) \
                    4 \
       	        } \
       	        4 \
     	        vertical { \
                    globalX \
                    4 \
                    globalY \
                    4 \
                    globalZ \
                    4 \
                    parallel \
 	            4 \
                    radial \
 	            4 \
 	        } \
            } \
        } \
 	20 \
 	vertical { \
 	    4 \
            ((height name - height node_label) / 2) \
            node_label \
            ((height name - height node_label) / 2) \
            4 \
            ((height globalX - height node1) / 2) \
            node1 \
            ((height globalX - height node1) / 2) \
            4 \
            ((height globalY - height node2) / 2) \
            node2 \
            ((height globalY - height node2) / 2) \
            4 \
            ((height globalZ - height node3) / 2) \
            node3 \
            ((height globalZ - height node3) / 2) \
            4 \
            ((height parallel - height node4) / 2) \
            node4 \
            ((height parallel - height node4) / 2) \
 	    4 \
 	} \
 	4 \
        vertical { \
            4 \
            ((height name - height magnitude_label) / 2) \
            magnitude_label \
            ((height name - height magnitude_label) / 2) \
            4 \
            ((height globalX - height magnitude1) / 2) \
            magnitude1 \
            ((height globalX - height magnitude1) / 2) \
            4 \
            ((height globalY - height magnitude2) / 2) \
            magnitude2 \
            ((height globalY - height magnitude2) / 2) \
            4 \
            ((height globalZ - height magnitude3) / 2) \
            magnitude3 \
            ((height globalZ - height magnitude3) / 2) \
            4 \
            ((height parallel - height magnitude4) / 2) \
            magnitude4 \
            ((height parallel - height magnitude4) / 2) \
            4 \
        } \
        4 \
     } \
     separator2 <+inf -100% *> \
     4 \
     horizontal { \
 	4 help 4 <+inf -100%> \
 	accept 4 <+inf -100%> \
 	dismiss 4 <+inf -100%> \
 	delete 4 <+inf -100%> \
 	new 4 <+inf -100%> \
 	copy 4 \
     } \
     4 \
}";

static Arg color_args [ ] = {
    {XtNborderColor, (XtArgVal) &highlight},
};

static Arg shell_args [ ] = {
    {XtNtitle,    (XtArgVal) NULL},
    {XtNiconName, (XtArgVal) NULL},
};

static Arg layout_args [ ] = {
    {XtNlayout, (XtArgVal) NULL},
};

static Arg viewport_args [ ] = {
    {XtNallowVert,   (XtArgVal) True},
    {XtNforceBars,   (XtArgVal) True},
    {XtNborderWidth, (XtArgVal) 0},
};

static Arg list_args [ ] = {
    {XtNdefaultColumns, (XtArgVal) 1},
    {XtNforceColumns,   (XtArgVal) 1},
    {XtNresize,		(XtArgVal) True},
    {XtNlist,		(XtArgVal) dummy_list},
};

static Arg text_args [ ] = {
    {XtNeditType,    (XtArgVal) XawtextEdit},
    {XtNborderWidth, (XtArgVal) 0},
    {XtNpieceSize,   (XtArgVal) 32},
    {XtNcursorName,  (XtArgVal) "left_ptr"},
};

static Arg node_args [ ] = {
    {XtNeditType,    (XtArgVal) XawtextEdit},
    {XtNborderWidth, (XtArgVal) 0},
    {XtNpieceSize,   (XtArgVal) 32},
    {XtNwidth,	     (XtArgVal) 40},
    {XtNcursorName,  (XtArgVal) "left_ptr"},
}; 

static Arg label_args [ ] = {
    {XtNlabel,       (XtArgVal) ""},
    {XtNborderWidth, (XtArgVal) 0},
};

static Arg toggle_args [ ] = {
    {XtNlabel,      (XtArgVal) " "},
};

static Arg core_args [ ] = {
    {XtNwidth,  (XtArgVal) 3},
    {XtNheight, (XtArgVal) 3},
};


/* Translation tables */

static String text_table =
"<Key>Return: LoadDialogAction(accept)\n\
 <Key>Escape: LoadDialogAction(dismiss)\n\
 Ctrl<Key>d:  LoadDialogAction(delete)\n\
 Ctrl<Key>c:  LoadDialogAction(copy)\n\
 Ctrl<Key>n:  LoadDialogAction(new)\n\
 Ctrl<Key>h:  LoadDialogAction(help)\n\
 <Btn1Down>:  SetFocus() select-start()";

static XtTranslations text_translations;


static String command_table =
"<Key>Return:  LoadDialogAction(accept)\n\
 <Key>Escape:  LoadDialogAction(dismiss)\n\
 Ctrl<Key>d:   LoadDialogAction(delete)\n\
 Ctrl<Key>c:   LoadDialogAction(copy)\n\
 Ctrl<Key>n:   LoadDialogAction(new)\n\
 Ctrl<Key>h:   LoadDialogAction(help)\n\
 <Key>space:   AutoRepeat(off) set()\n\
 <KeyUp>space: AutoRepeat(saved) notify() unset()";

static XtTranslations command_translations;


static String toggle_table = 
"<Key>Return: LoadDialogAction(accept)\n\
 <Key>Escape: LoadDialogAction(dismiss)\n\
 Ctrl<Key>d:  LoadDialogAction(delete)\n\
 Ctrl<Key>c:  LoadDialogAction(copy)\n\
 Ctrl<Key>n:  LoadDialogAction(new)\n\
 Ctrl<Key>h:  LoadDialogAction(help)\n\
 <Key>space:  ToggleAction()";

static XtTranslations toggle_translations;


static String viewport_table =
"<Key>Return: LoadDialogAction(accept)\n\
 <Key>Escape: LoadDialogAction(dismiss)\n\
 Ctrl<Key>d:  LoadDialogAction(delete)\n\
 Ctrl<Key>c:  LoadDialogAction(copy)\n\
 Ctrl<Key>n:  LoadDialogAction(new)\n\
 Ctrl<Key>h:  LoadDialogAction(help)\n\
 <Btn1Down>:  SetFocus()";

static XtTranslations viewport_translations;


static String help_table =
"<Key>Return: LoadDialogAction(accept)\n\
 <Key>Escape: LoadDialogAction(dismiss)\n\
 Ctrl<Key>d:  LoadDialogAction(delete)\n\
 Ctrl<Key>c:  LoadDialogAction(copy)\n\
 Ctrl<Key>n:  LoadDialogAction(new)\n\
 Ctrl<Key>h:  LoadDialogAction(help)\n\
 <Key>space:  PostMenu()";

static XtTranslations help_translations;


/* Help message. */

static String help_message ="\
The load form is used to define, edit and delete loads (distributed loads).  \
The name box displays the name of the current load and can be used to \
name (or rename) a load.  Use the list to change the current load.  The \
entry fields to the right define the nodes and magnitudes of the current \
load.  The toggle buttons control the load direction.  Use the 'Accept' \
button to register your changes.  'Delete' erases the current load.  'New' \
empties all fields.  'Copy' empties the name field only.";


/************************************************************************
 * Function:	AppendLoadName						*
 *									*
 * Description:	Appends the load name to the array of names.  The	*
 *		index of the active load is also set.			*
 ************************************************************************/

static int AppendLoadName (Distributed item)
{
    if (dialog -> active == item)
	list_index = num_loads;

    dialog -> loads [num_loads ++] = XtNewString(item -> name.c_str());
    return 0;
}


/************************************************************************
 * Function:	SetRadioState						*
 *									*
 * Description: Sets a selected load direction.				*
 ************************************************************************/

static void SetRadioState (Widget w, XtPointer client_data, XtPointer call_data)
{
   unsigned		mode;
   Arg			args [1];
   Boolean		state;
   static Widget	prev_widget = NULL;

   mode = *((unsigned *) client_data);

   	/*
	 * Figure out if this widget is being turned on or off.
	 * Basically, we enforce one of many by never allowing the
	 * user to turn a toggle off.
	 */

   if (mode == Forced) {
      if (prev_widget != w) {
         XtSetArg (args[0], XtNstate, True);
         XtSetValues (w, args, 1);
         if (prev_widget != NULL) {
            XtSetArg (args[0], XtNstate, False);
            XtSetValues (prev_widget, args, 1);
         }
      }
   }
   else {
      XtSetArg (args[0], XtNstate, &state);
      XtGetValues (w, args, 1);

      if (!state) {
         XtSetArg (args[0], XtNstate, True);
         XtSetValues (w, args, 1);
      }
      else if (prev_widget != NULL) {   
         XtSetArg (args[0], XtNstate, False);
         XtSetValues (prev_widget, args, 1);
      }
   }

   prev_widget = w;
}


/************************************************************************
 * Function:	GetRadioState						*
 *									*
 * Description:	Determine which load direction is selected.		*
 ************************************************************************/

static Direction GetRadioState (LoadDialog loadd)
{
   Boolean	state;
   Arg		args[1];
   
   XtSetArg (args [0], XtNstate, &state);

   XtGetValues (loadd -> localX, args, 1);
   if (state) return LocalX;
   XtGetValues (loadd -> localY, args, 1);
   if (state) return LocalY;
   XtGetValues (loadd -> localZ, args, 1);
   if (state) return LocalZ;
   XtGetValues (loadd -> globalX, args, 1);
   if (state) return GlobalX;
   XtGetValues (loadd -> globalY, args, 1);
   if (state) return GlobalY;
   XtGetValues (loadd -> globalZ, args, 1);
   if (state) return GlobalZ;
   XtGetValues (loadd -> parallel, args, 1);
   if (state) return Parallel;
   XtGetValues (loadd -> perpendicular, args, 1);
   if (state) return Perpendicular;
   XtGetValues (loadd -> radial, args, 1);
   if (state) return Radial;
   XtGetValues (loadd -> axial, args, 1);
   if (state) return Axial;

   return (Direction) 0; 
}


/************************************************************************
 * Function:	ToggleAction						*
 *									*
 * Description:	Callback to set the radio state.			*
 ************************************************************************/

static void ToggleAction (Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    SetRadioState (w, &Forced, NULL);
}

 
/************************************************************************
 * Function:	Action							*
 *									*
 * Description:	An action procedure which emulates pressing of the	*
 *		specified button.					*	
 ************************************************************************/

static void Action (Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    if (XtClass (w) == topLevelShellWidgetClass)
	w = XtNameToWidget (w, "layout.dismiss");
    else
	w = XtNameToWidget (XtParent (w), params [0]);

    if (!strcmp (XtName (w), "help"))
	XtCallActionProc (w, "PostMenu", event, NULL, 0);
    else
	XtCallCallbacks (w, XtNcallback, NULL);
}


/************************************************************************
 * Function:	Change							*
 *									*
 * Description:	Changes the displayed values to either the currently	*
 *		selected load if the widget is not null, or the active	*
 *		load if the widget is null.  The newly displayed load	*
 *		is made the active load and any new/copy operation is	*
 *		canceled.						*
 ************************************************************************/

static void Change (Widget w, XtPointer client_data, XtPointer call_data)
{
    unsigned		 i;
    char		 buffer [32];
    Distributed		 active;
    LoadDialog		 loadd;
    XawListReturnStruct	*info;


    loadd = (LoadDialog) client_data;


    /* Retrieve the active load from the tree if selected. */

    if (w != NULL) {
        info = (XawListReturnStruct *) call_data;
        if (info -> list_index == XAW_LIST_NONE)
            return;
        
        loadd->active = SetSearch(*(loadd->tree), info->string);
    }

    active = loadd -> active;
    loadd -> new_copy = False;


    /* Update all of the text entries. */

    SetTextString (loadd -> name, active -> name.c_str());

    for (i = 1 ; i <= 4 ; i++) {
       if (i <= active -> value.size()) {
          sprintf (buffer, "%g", active -> value [i].magnitude);
          SetTextString (loadd -> magnitude[i-1], buffer);

          sprintf (buffer, "%d", active -> value [i].node);
          SetTextString (loadd -> nodenum[i-1], buffer);
       }
       else {
          SetTextString (loadd -> magnitude[i-1], "");
          SetTextString (loadd -> nodenum[i-1], "");
       }
    }

    switch (active -> direction) {
       case LocalX:
          SetRadioState (loadd -> localX, &Forced, NULL);
          break;
       case LocalY:
          SetRadioState (loadd -> localY, &Forced, NULL);
          break;
       case LocalZ:
          SetRadioState (loadd -> localZ, &Forced, NULL);
          break;
       case GlobalX: 
          SetRadioState (loadd -> globalX, &Forced, NULL);
          break;
       case GlobalY: 
          SetRadioState (loadd -> globalY, &Forced, NULL);
          break;
       case GlobalZ: 
          SetRadioState (loadd -> globalZ, &Forced, NULL);
          break;
       case Parallel: 
          SetRadioState (loadd -> parallel, &Forced, NULL);
          break;
       case Perpendicular: 
          SetRadioState (loadd -> perpendicular, &Forced, NULL);
          break;
       case Radial: 
          SetRadioState (loadd -> radial, &Forced, NULL);
          break;
       case Axial: 
          SetRadioState (loadd -> axial, &Forced, NULL);
          break;
       default:
          SetRadioState (loadd -> localX, &Forced, NULL);
    }
}


/************************************************************************
 * Function:	Accept							*
 *									*
 * Description:	Accepts changes made to the currently displayed load.	*
 *		If the name is empty or a duplicate name is given then	*
 *		an error is reported.  Otherwise, a new load is		*
 *		created if a new/copy operation is in effect.  The	*
 *		load is then redisplayed to correct any invalid		*
 *		entries.						*
 ************************************************************************/

static void Accept (Widget w, XtPointer client_data, XtPointer call_data)
{
    String		value;
    distributed_t  dummy;
    distributed_t  old;
    Distributed	 	active;
    Boolean	 	duplicate;
    char		*ptr;
    unsigned		count;
    unsigned		valid_pairs [4];
    unsigned		i;
    long		temp;
    LoadDialog	 	loadd;
    LoadDialogInfo	info;


    loadd = (LoadDialog) client_data;


    /* Retrieve the name of the load. */

    dummy.name = GetTextString (loadd -> name);
    Distributed found = SetSearch(*loadd->tree, dummy.name);
    duplicate = found && (found != loadd -> active || loadd -> new_copy);


    /* Check for a duplicate name. */

    if (!dummy.name [0] || duplicate) {
	XBell (XtDisplay (loadd -> name), 0);
	SetFocus (loadd -> name);
	if (!loadd -> new_copy)
	    SetTextString (loadd -> name, loadd -> active -> name.c_str());
	else
	    SetTextString (loadd -> name, "");

    } else {
        count = 0;
        for (i = 0 ; i < 4 ; i++) {
	   value = GetTextString (loadd -> nodenum [i]);
           if (strcmp (value, ""))  {
              temp = strtol (value, &ptr, 10);
              if (*ptr == 0) 
                 valid_pairs [count++] = i;
           }
        }
        
        if (!loadd -> new_copy && count > loadd -> active -> value.size()) {
            loadd->active->value.resize(count);
        }

	/* Create a new load or new name as needed. */

	if (loadd -> new_copy)
        loadd -> active.reset(new distributed_t(dummy.name.c_str(), count));
	else if (strcmp (loadd -> active -> name.c_str(), dummy.name.c_str())) {
        Distributed old(new distributed_t(loadd -> active -> name.c_str()));
        loadd->tree->erase(old);
        loadd->active->name = dummy.name;
        loadd->tree->insert(loadd->active);
	}

	active = loadd -> active;

        for (i = 1 ; i <= count ; i++) {
	   active -> value [i].magnitude = exptod (GetTextString
		              (loadd -> magnitude [valid_pairs [i - 1]]), NULL);
	   active -> value [i].node = atoi (GetTextString
			      (loadd -> nodenum [valid_pairs [i - 1]]));
        }

        active -> direction = GetRadioState (loadd);
        active -> value.resize(count);
        
	if (loadd -> new_copy)
        loadd->tree->insert(loadd->active);

	if (loadd -> callback != NULL) {
	    w = loadd -> shell;
	    info.dialog  = loadd;
	    info.load    = loadd -> active;
	    info.deleted = False;
	    info.proceed = True;
	    loadd -> callback (w, loadd -> closure, &info);
	}

	LoadDialogUpdate (loadd, loadd -> tree);
    }
}


/************************************************************************
 * Function:	Dismiss							*
 *									*
 * Description:	Pops down the dialog box.				*
 ************************************************************************/

static void Dismiss (Widget w, XtPointer client_data, XtPointer call_data)
{
    LoadDialog loadd;


    loadd = (LoadDialog) client_data;
    XtPopdown (loadd -> shell);
}


/************************************************************************
 * Function:	Delete							*
 *									*
 * Description:	Deletes the active load if a new/copy operation is not	*
 *		in effect.  The dialog is then updated.			*
 ************************************************************************/

static void Delete (Widget w, XtPointer client_data, XtPointer call_data)
{
    LoadDialog     loadd;
    LoadDialogInfo info;


    loadd = (LoadDialog) client_data;

    if (!loadd -> new_copy) {
	if (loadd -> callback != NULL) {
	    w = loadd -> shell;
	    info.dialog  = loadd;
	    info.load	 = loadd -> active;
	    info.deleted = True;
	    info.proceed = True;
	    loadd -> callback (w, loadd -> closure, &info);
	    if (info.proceed == False)
		return;
	}

    loadd->tree->erase(loadd->active);
	loadd -> active.reset();
    }

    LoadDialogUpdate (loadd, loadd -> tree);
}


/************************************************************************
 * Function:	Copy							*
 *									*
 * Description:	Clears the name entry only and sets the flag indicating	*
 *		that a new/copy operation is in effect.			*
 ************************************************************************/

static void Copy (Widget w, XtPointer client_data, XtPointer call_data)
{
    LoadDialog loadd;


    loadd = (LoadDialog) client_data;

    loadd -> new_copy = True;
    SetFocus (loadd -> name);
    XawListUnhighlight (loadd -> list);
    SetTextString (loadd -> name, "");
}


/************************************************************************
 * Function:	New							*
 *									*
 * Description:	Clears all entries in the load dialog and sets the	*
 *		flag indicating that a new/copy operation is in effect.	*
 ************************************************************************/

static void New (Widget w, XtPointer client_data, XtPointer call_data)
{
    unsigned   i;
    LoadDialog loadd;


    loadd = (LoadDialog) client_data;

    Copy (NULL, client_data, NULL);
    for (i = 0 ; i < 4 ; i++) {
       SetTextString (loadd -> nodenum [i], "");
       SetTextString (loadd -> magnitude [i], "");
    }

    SetRadioState (loadd -> localX, &Forced, NULL);
}


/************************************************************************
 * Function:	LoadDialogCreate					*
 *									*
 * Description:	Creates a new load dialog.  You would never want to	*
 *		have more than one of these but the interface is kept	*
 *		consistent with those of the other dialogs.		*
 ************************************************************************/

LoadDialog LoadDialogCreate (Widget parent, String name, String title, XtCallbackProc callback, XtPointer closure)
{
    Cardinal		i;
    Arg			args [2];
    Widget		group [24];
    LoadDialog		loadd;
    char		name_buff [40];
    Dimension		width;
    Position		x;
    static XtAppContext	app_context = NULL;
    static XtActionsRec	actions [ ] = {{"LoadDialogAction", Action},
                                       {"ToggleAction", ToggleAction}};


    /* Perform one time initialization. */

    if (app_context == NULL) {
	app_context = XtWidgetToApplicationContext (parent);
	XtAppAddActions (app_context, actions, XtNumber (actions));
	AddAutoRepeatAction (app_context);

        layout_args [0].value = StringToLayout (parent, layout_string);

	text_translations = XtParseTranslationTable (text_table);
	command_translations = XtParseTranslationTable (command_table);
	viewport_translations = XtParseTranslationTable (viewport_table);
        toggle_translations = XtParseTranslationTable (toggle_table);
	help_translations = XtParseTranslationTable (help_table);
    }


    /* Create the load dialog and its widgets. */

    XtSetArg (shell_args [0], XtNtitle, title);
    XtSetArg (shell_args [1], XtNiconName, title);

    loadd = new struct load_dialog;

    loadd -> callback = callback;

    loadd -> closure  = closure;

    loadd -> loads    = NULL;

    loadd -> active.reset();

    loadd -> shell    = XtCreatePopupShell (name,
			 topLevelShellWidgetClass, parent,
			 shell_args, XtNumber (shell_args));

    loadd -> layout   = XtCreateManagedWidget ("layout",
			 layoutWidgetClass, loadd -> shell,
			 layout_args, XtNumber (layout_args));

    loadd -> viewport = XtCreateManagedWidget ("viewport",
			 viewportWidgetClass, loadd -> layout,
			 viewport_args, XtNumber (viewport_args));

    loadd -> list     = XtCreateManagedWidget ("list",
			 listWidgetClass, loadd -> viewport,
			 list_args, XtNumber (list_args));

    loadd -> name     = XtCreateManagedWidget ("name",
			 asciiTextWidgetClass, loadd -> layout,
			 text_args, XtNumber (text_args));


    for (i = 0 ; i < 4 ; i++) {
       sprintf (name_buff,"node%d",i+1);
       loadd -> nodenum[i] = XtCreateManagedWidget (name_buff,
                               asciiTextWidgetClass, loadd -> layout,
                               node_args, XtNumber (node_args));

       sprintf (name_buff,"magnitude%d",i+1);
       loadd -> magnitude[i] = XtCreateManagedWidget (name_buff,
                                 asciiTextWidgetClass, loadd -> layout,
                                 text_args, XtNumber (text_args));
    }


    loadd -> localX = XtCreateManagedWidget ("localX",
                        toggleWidgetClass, loadd -> layout,
                        toggle_args, XtNumber (toggle_args));

    loadd -> localY = XtCreateManagedWidget ("localY",
                        toggleWidgetClass, loadd -> layout,
                        toggle_args, XtNumber (toggle_args));

    loadd -> localZ = XtCreateManagedWidget ("localZ",
                        toggleWidgetClass, loadd -> layout,
                        toggle_args, XtNumber (toggle_args));

    loadd -> globalX = XtCreateManagedWidget ("globalX",
                        toggleWidgetClass, loadd -> layout,
                        toggle_args, XtNumber (toggle_args));

    loadd -> globalY = XtCreateManagedWidget ("globalY",
                        toggleWidgetClass, loadd -> layout,
                        toggle_args, XtNumber (toggle_args));

    loadd -> globalZ = XtCreateManagedWidget ("globalZ",
                        toggleWidgetClass, loadd -> layout,
                        toggle_args, XtNumber (toggle_args));

    loadd -> perpendicular = XtCreateManagedWidget ("perpendicular",
                              toggleWidgetClass, loadd -> layout,
                              toggle_args, XtNumber (toggle_args));

    loadd -> parallel = XtCreateManagedWidget ("parallel",
                          toggleWidgetClass, loadd -> layout,
                          toggle_args, XtNumber (toggle_args));

    loadd -> axial = XtCreateManagedWidget ("axial",
                          toggleWidgetClass, loadd -> layout,
                          toggle_args, XtNumber (toggle_args));

    loadd -> radial = XtCreateManagedWidget ("radial",
                          toggleWidgetClass, loadd -> layout,
                          toggle_args, XtNumber (toggle_args));


    loadd -> accept   = XtCreateManagedWidget ("accept",
			 commandWidgetClass, loadd -> layout,
			 NULL, 0);

    loadd -> dismiss  = XtCreateManagedWidget ("dismiss",
			 commandWidgetClass, loadd -> layout,
			 NULL, 0);

    loadd -> nuke   = XtCreateManagedWidget ("delete",
			 commandWidgetClass, loadd -> layout,
			 NULL, 0);

    loadd -> nu      = XtCreateManagedWidget ("new",
			 commandWidgetClass, loadd -> layout,
			 NULL, 0);

    loadd -> copy     = XtCreateManagedWidget ("copy",
			 commandWidgetClass, loadd -> layout,
			 NULL, 0);

    loadd -> help     = CreateHelpButton (loadd -> layout, "help");


    for (i = 0; i < XtNumber (labels); i ++) {
	label_args [0].value = (XtArgVal) labels [i];
	XtCreateManagedWidget (names [i], labelWidgetClass,
		loadd -> layout, label_args, XtNumber (label_args));
    }


    XtCreateManagedWidget ("separator1", coreWidgetClass,
			loadd -> layout, core_args, XtNumber (core_args));

    XtCreateManagedWidget ("separator2", coreWidgetClass,
			loadd -> layout, core_args, XtNumber (core_args));


    /* Create a tab group for the load dialog. */

    group [0]  = loadd -> name;
    group [1]  = loadd -> viewport;
    group [2]  = loadd -> localX;
    group [3]  = loadd -> localY;
    group [4]  = loadd -> localZ;
    group [5]  = loadd -> perpendicular;
    group [9]  = loadd -> axial; 
    group [6]  = loadd -> globalX;
    group [7]  = loadd -> globalY;
    group [8]  = loadd -> globalZ;
    group [9]  = loadd -> parallel; 
    group [9]  = loadd -> radial; 
    for (i = 0 ; i < 4 ; i++) {
       group [10 + 2*i] = loadd -> nodenum[i];
       group [10 + 2*i + 1] = loadd -> magnitude[i];
    }
    group [18] = loadd -> help;
    group [19] = loadd -> accept;
    group [20] = loadd -> dismiss;
    group [21] = loadd -> nuke;
    group [22] = loadd -> nu;
    group [23] = loadd -> copy;

    XtGetValues (loadd -> layout, color_args, XtNumber (color_args));
    CreateTabGroup (loadd -> shell, group, XtNumber (group), highlight, True);
    XtRealizeWidget (loadd -> shell);
    SetFocus (loadd -> name);

    XtSetArg (args [0], XtNwidth, &width);
    XtGetValues (loadd -> layout, args, 1);
    XtSetArg (args [0], XtNx, &x);
    XtGetValues (loadd -> help, args, 1);
    UpdateHelpMessage (loadd -> help, help_message, width - 2 * x);



    /* Add the translations to each widget. */

    AddDeleteWindowProtocol   (loadd -> shell, "LoadDialogAction()");
    ListAddCursorTranslations (loadd -> viewport);

    XtOverrideTranslations (loadd -> name,	text_translations);
    for (i = 0 ; i < 4 ; i++) {
       XtOverrideTranslations (loadd -> nodenum[i],   text_translations);
       XtOverrideTranslations (loadd -> magnitude[i], text_translations);
    }

    XtOverrideTranslations (loadd -> localX,    toggle_translations);
    XtOverrideTranslations (loadd -> localY,    toggle_translations);
    XtOverrideTranslations (loadd -> localZ,    toggle_translations);
    XtOverrideTranslations (loadd -> globalX,   toggle_translations);
    XtOverrideTranslations (loadd -> globalY,   toggle_translations);
    XtOverrideTranslations (loadd -> globalZ,   toggle_translations);
    XtOverrideTranslations (loadd -> perpendicular, toggle_translations);
    XtOverrideTranslations (loadd -> parallel,      toggle_translations);
    XtOverrideTranslations (loadd -> radial,      toggle_translations);
    XtOverrideTranslations (loadd -> axial,      toggle_translations);
    XtOverrideTranslations (loadd -> accept,	command_translations);
    XtOverrideTranslations (loadd -> dismiss,	command_translations);
    XtOverrideTranslations (loadd -> nuke,	command_translations);
    XtOverrideTranslations (loadd -> nu,	command_translations);
    XtOverrideTranslations (loadd -> copy,	command_translations);
    XtOverrideTranslations (loadd -> viewport,	viewport_translations);
    XtOverrideTranslations (loadd -> help,	help_translations);


    /* Add the necessary callbacks. */

    XtAddCallback (loadd -> localX,  XtNcallback, SetRadioState, &Button);
    XtAddCallback (loadd -> localY,  XtNcallback, SetRadioState, &Button);
    XtAddCallback (loadd -> localZ,  XtNcallback, SetRadioState, &Button);
    XtAddCallback (loadd -> globalX, XtNcallback, SetRadioState, &Button);
    XtAddCallback (loadd -> globalY, XtNcallback, SetRadioState, &Button);
    XtAddCallback (loadd -> globalZ, XtNcallback, SetRadioState, &Button);
    XtAddCallback (loadd -> parallel,XtNcallback,SetRadioState, &Button);
    XtAddCallback (loadd -> radial,  XtNcallback,SetRadioState, &Button);
    XtAddCallback (loadd -> axial,   XtNcallback,SetRadioState, &Button);
    XtAddCallback (loadd -> perpendicular, XtNcallback,SetRadioState, &Button);

    XtAddCallback (loadd -> list,    XtNcallback, Change,  (XtPointer) loadd);
    XtAddCallback (loadd -> accept,  XtNcallback, Accept,  (XtPointer) loadd);
    XtAddCallback (loadd -> dismiss, XtNcallback, Dismiss, (XtPointer) loadd);
    XtAddCallback (loadd -> nuke,  XtNcallback, Delete,  (XtPointer) loadd);
    XtAddCallback (loadd -> nu,     XtNcallback, New,     (XtPointer) loadd);
    XtAddCallback (loadd -> copy,    XtNcallback, Copy,    (XtPointer) loadd);


    return loadd;
}


/************************************************************************
 * Function:	LoadDialogPopup						*
 *									*
 * Description:	Pops up the specified load dialog.			*
 ************************************************************************/

void LoadDialogPopup (LoadDialog loadd)
{
    XtPopup (loadd -> shell, XtGrabNone);
}


/************************************************************************
 * Function:	LoadDialogActive					*
 *									*
 * Description:	Returns the currently active load.			*
 ************************************************************************/

Distributed LoadDialogActive (LoadDialog loadd)
{
    return loadd -> active;
}


/************************************************************************
 * Function:	LoadDialogDisplay					*
 *									*
 * Description:	Displays a specified load.				*
 ************************************************************************/

void LoadDialogDisplay (LoadDialog loadd, Distributed load)
{
    loadd -> active = load;
    LoadDialogUpdate (loadd, loadd -> tree);
}


/************************************************************************
 * Function:	LoadDialogUpdate					*
 *									*
 * Description:	Updates the specified load dialog with the specified	*
 *		tree.  If no active load exists, the first load is	*
 *		made active.  If no active load still exists then	*
 *		a new operation is performed.  Otherwise a change	*
 *		operation is performed to display the active values.	*
 ************************************************************************/

void LoadDialogUpdate (LoadDialog loadd, Problem::DistributedSet *tree)
{
    Cardinal nbytes;


    /* Determine a new active load if necessary. */

    if (tree == NULL)
	tree = loadd -> tree;

    if (loadd -> active == NULL || tree != loadd -> tree)
        loadd -> active = SetMinimum(*tree);


    /* Construct the array of load names. */

    num_loads = 0;
    list_index = -1;
    dialog = loadd;
    loadd -> tree = tree;
    loadd -> new_copy = False;

    nbytes = (loadd->tree->size() + 1) * sizeof (String);
    loadd -> loads = (String *) XtRealloc ((char *) loadd -> loads, nbytes);

    std::for_each(loadd->tree->begin(), loadd->tree->end(), AppendLoadName);
    loadd -> loads [num_loads] = NULL;


    /* Update the list widget. */

    XawListChange (loadd -> list, loadd -> loads, 0, 0, True);

    if (list_index >= 0)
	XawListHighlight (loadd -> list, list_index);


    /* Update the text entries. */

    if (loadd -> active == NULL) 
	New (NULL, (XtPointer) loadd, NULL);
    else
	Change (NULL, (XtPointer) loadd, NULL);
}
