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
 * File:	force.c							*
 *									*
 * Description:	This file contains the public and private function and	*
 *		type definitions for the force dialog box.		*
 ************************************************************************/

# include <stdio.h>
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Shell.h>
# include <X11/Xaw/AsciiText.h>
# include <X11/Xaw/Command.h>
# include <X11/Xaw/Label.h>
# include <X11/Xaw/List.h>
# include <X11/Xaw/Viewport.h>
# include <X11/Xaw/Toggle.h>
# include "Layout.h"
# include "Force.h"
# include "TabGroup.h"
# include "util.h"
# include "fe.h"
# include "objects.h"
# include "procedures.h"

# ifndef X_NOT_STDC_ENV
# include <stdlib.h>
# endif


struct force_dialog {
    Widget         shell;	/* topLevelShell  <specified>	*/
    Widget         layout;	/*	Layout  layout		*/
    Widget         name;	/*	     AsciiText  name	*/
    Widget         force_x;	/*	     AsciiText  forceX	*/
    Widget         force_y;	/*	     AsciiText  forceY	*/
    Widget         force_z;	/*	     AsciiText  forceZ	*/
    Widget         moment_x;	/*	     AsciiText  momentX	*/
    Widget         moment_y;	/*	     AsciiText  momentY	*/
    Widget         moment_z;	/*	     AsciiText  momentZ	*/
    Widget	   time_domain;	/*	     Toggle time_domain */
    Widget	   freq_domain; /*	     Toggle freq_domain */
    Widget	   plot_force;  /*	     Command plot_force */
    Widget	   plot_moment; /*	     Command plot_moment */
    Widget         viewport;	/*	     Viewport  viewport	*/
    Widget         list;	/*		  List  list	*/
    Widget         help;	/*	     MenuButton  help	*/
    Widget         accept;	/*	     Command  accept	*/
    Widget         dismiss;	/*	     Command  dismiss	*/
    Widget         delete;	/*	     Command  delete	*/
    Widget         new;		/*	     Command  new	*/
    Widget         copy;	/*	     Command  copy	*/
    XtCallbackProc callback;
    XtPointer	   closure;
    String	  *forces;
    Force	   active;
    Boolean        new_copy;
    Tree           tree;
    unsigned	   domain;
};

static String labels [ ] = {
    "Name:", "X:", "Y:", "Z:", "Forces", "Moments", "time", "frequency" 
};

static String names [ ] = {
    "nameLabel", "x", "y", "z", "forces", "moments", "time", "frequency"
};

static ForceDialog dialog;
static Cardinal	   num_forces;
static int	   list_index;


/* Resources */

static Pixel highlight;

static String dummy_list [ ] = {
    NULL
};

static String layout_string = 
"vertical { \
     horizontal { \
 	4 \
 	vertical { \
	    4 \
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
        vertical { \
            4 \
            ((height name - height time_domain) / 2) \
            horizontal { \
                4 \
                time_domain \
                2 \
                time \
                4 <+inf -100%> \
                freq_domain \
                2 \
                frequency \
                4 \
            } \
            ((height name - height time_domain) / 2) \
            4 \
            separator3 <+inf -100% *> \
            horizontal { \
 	        vertical { \
         	    4 \
        	    height forces \
        	    4 \
        	    ((height forceX - height x) / 2) \
         	    x \
	            ((height forceX - height x) / 2) \
        	    4 \
        	    ((height forceY - height y) / 2) \
         	    y \
	            ((height forceY - height y) / 2) \
        	    4 \
        	    ((height forceZ - height z) / 2) \
         	    z \
	            ((height forceZ - height z) / 2) \
         	    4 <+inf> \
         	} \
         	4 \
         	vertical { \
         	    4 \
         	    forces <+inf *> \
        	    4 \
         	    forceX <+inf *> \
        	    4 \
         	    forceY <+inf *> \
	            4 \
         	    forceZ <+inf *> \
         	    4 <+inf> \
         	} \
         	4 \
         	vertical { \
         	    4 \
         	    moments <+inf *> \
        	    4 \
         	    momentX <+inf *> \
	            4 \
         	    momentY <+inf *> \
        	    4 \
         	    momentZ <+inf *> \
 	            4 <+inf> \
         	} \
         	4 \
            } \
            4 \
            separator4 <+inf -100% *> \
            4 \
            horizontal { \
                4 \
                plot_force \
                4 <+inf> \
                plot_moment \
                4 \
            } \
            4 \
         } \
     } \
     separator2 <+inf -100% *> \
     4 \
     horizontal { \
 	4 \
 	help \
 	4 <+inf -100%> \
 	accept \
 	4 <+inf -100%> \
 	dismiss \
 	4 <+inf -100%> \
 	delete \
 	4 <+inf -100%> \
 	new \
 	4 <+inf -100%> \
 	copy \
 	4 \
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
"<Key>Return: ForceDialogAction(accept)\n\
 <Key>Escape: ForceDialogAction(dismiss)\n\
 Ctrl<Key>d:  ForceDialogAction(delete)\n\
 Ctrl<Key>c:  ForceDialogAction(copy)\n\
 Ctrl<Key>n:  ForceDialogAction(new)\n\
 Ctrl<Key>h:  ForceDialogAction(help)\n\
 <Btn1Down>:  SetFocus() select-start()";

static XtTranslations text_translations;


static String command_table =
"<Key>Return:  ForceDialogAction(accept)\n\
 <Key>Escape:  ForceDialogAction(dismiss)\n\
 Ctrl<Key>d:   ForceDialogAction(delete)\n\
 Ctrl<Key>c:   ForceDialogAction(copy)\n\
 Ctrl<Key>n:   ForceDialogAction(new)\n\
 Ctrl<Key>h:   ForceDialogAction(help)\n\
 <Key>space:   AutoRepeat(off) set()\n\
 <KeyUp>space: AutoRepeat(saved) notify() unset()";

static XtTranslations command_translations;


static String viewport_table =
"<Key>Return: ForceDialogAction(accept)\n\
 <Key>Escape: ForceDialogAction(dismiss)\n\
 Ctrl<Key>d:  ForceDialogAction(delete)\n\
 Ctrl<Key>c:  ForceDialogAction(copy)\n\
 Ctrl<Key>n:  ForceDialogAction(new)\n\
 Ctrl<Key>h:  ForceDialogAction(help)\n\
 <Btn1Down>:  SetFocus()";

static XtTranslations viewport_translations;


static String toggle_table =
"<Key>Return: ForceDialogAction(accept)\n\
 <Key>Escape: ForceDialogAction(dismiss)\n\
 Ctrl<Key>d:  ForceDialogAction(delete)\n\
 Ctrl<Key>c:  ForceDialogAction(copy)\n\
 Ctrl<Key>n:  ForceDialogAction(new)\n\
 Ctrl<Key>h:  ForceDialogAction(help)\n\
 <Key>space:  toggle() notify()";

static XtTranslations toggle_translations;


static String help_table =
"<Key>Return: ForceDialogAction(accept)\n\
 <Key>Escape: ForceDialogAction(dismiss)\n\
 Ctrl<Key>d:  ForceDialogAction(delete)\n\
 Ctrl<Key>c:  ForceDialogAction(copy)\n\
 Ctrl<Key>n:  ForceDialogAction(new)\n\
 Ctrl<Key>h:  ForceDialogAction(help)\n\
 <Key>space:  PostMenu()";

static XtTranslations help_translations;


/* Help message. */

static String help_message ="\
The force form is used to define, edit and delete forces (point loads).  \
The name box displays the name of the current force and can be used to \
name (or rename) a force.  Use the list to change the current force.  The \
toggle buttons flip the display of force entries between time-domain forces \
and frequency-domain forces (spectra).  The six entry fields define the \
current value or expression in the current domain for the corresponding DOF. \
Use the 'Accept' button to register your changes. 'Delete' erases the \
current force. 'New' empties all fields. 'Copy' empties the name field only.";


/************************************************************************
 * Function:	AppendForceName						*
 *									*
 * Description:	Appends the force name to the array of names.  The	*
 *		index of the active force is also set.			*
 ************************************************************************/

static int AppendForceName (Item item)
{
    if (dialog -> active == (Force) item)
	list_index = num_forces;

    dialog -> forces [num_forces ++] = ((Force) item) -> name;
    return 0;
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

static void LoadForceValues (ForceDialog forced)
{
    Boolean	state;
    Arg		args [1];
    Force	active;
    char	buffer [256];

    active = forced -> active;
    if (active == NULL)
       return;

    XtSetArg (args[0], XtNstate, &state);
    XtGetValues (forced -> time_domain, args, 1);

    if (state) {
       if (active -> force[1].expr != NULL)
          SetTextString (forced -> force_x, active -> force[1].text);
       else {
          sprintf (buffer, (active -> force [1].value ? "%g" : ""), active -> force [1].value);
          SetTextString (forced -> force_x, buffer);
       }

       if (active -> force[2].expr != NULL)
          SetTextString (forced -> force_y, active -> force[2].text);
       else {
          sprintf (buffer, (active -> force [2].value ? "%g" : ""), active -> force [2].value);
          SetTextString (forced -> force_y, buffer);
       }

       if (active -> force[3].expr != NULL)
          SetTextString (forced -> force_z, active -> force[3].text);
       else {
          sprintf (buffer, (active -> force [3].value ? "%g" : ""), active -> force [3].value);
          SetTextString (forced -> force_z, buffer);
       }

       if (active -> force[4].expr != NULL)
          SetTextString (forced -> moment_x, active -> force[4].text);
       else {
          sprintf (buffer, (active -> force [4].value ? "%g" : ""), active -> force [4].value);
          SetTextString (forced -> moment_x, buffer);
       }

       if (active -> force[5].expr != NULL)
          SetTextString (forced -> moment_y, active -> force[5].text);
       else {
          sprintf (buffer, (active -> force [5].value ? "%g" : ""), active -> force [5].value);
          SetTextString (forced -> moment_y, buffer);
       }

       if (active -> force[6].expr != NULL)
          SetTextString (forced -> moment_z, active -> force[6].text);
       else {
          sprintf (buffer, (active -> force [6].value ? "%g" : ""), active -> force [6].value);
          SetTextString (forced -> moment_z, buffer);
       }
    }
    else {
       if (active -> spectrum[1].expr != NULL) 
          SetTextString (forced -> force_x, active -> spectrum[1].text);
       else {
          sprintf (buffer, (active -> spectrum [1].value ? "%g" : ""), active -> spectrum [1].value);
          SetTextString (forced -> force_x, buffer);
       }

       if (active -> spectrum[2].expr != NULL)
          SetTextString (forced -> force_y, active -> spectrum[2].text);
       else {
          sprintf (buffer, (active -> spectrum [2].value ? "%g" : ""), active -> spectrum [2].value);
          SetTextString (forced -> force_y, buffer);
       }

       if (active -> spectrum[3].expr != NULL)
          SetTextString (forced -> force_z, active -> spectrum[3].text);
       else {
          sprintf (buffer, (active -> spectrum [3].value ? "%g" : ""), active -> spectrum [3].value);
          SetTextString (forced -> force_z, buffer);
       }

       if (active -> spectrum[4].expr != NULL)
          SetTextString (forced -> moment_x, active -> spectrum[4].text);
       else {
          sprintf (buffer, (active -> spectrum [4].value ? "%g" : ""), active -> spectrum [4].value);
          SetTextString (forced -> moment_x, buffer);
       }

       if (active -> spectrum[5].expr != NULL)
          SetTextString (forced -> moment_y, active -> spectrum[5].text);
       else {
          sprintf (buffer, (active -> spectrum [5].value ? "%g" : ""), active -> spectrum [5].value);
          SetTextString (forced -> moment_y, buffer);
       }

       if (active -> spectrum[6].expr != NULL)
          SetTextString (forced -> moment_z, active -> spectrum[6].text);
       else {
          sprintf (buffer, (active -> spectrum [6].value ? "%g" : ""), active -> spectrum [6].value);
          SetTextString (forced -> moment_z, buffer);
       }
    }
}

/************************************************************************
 * Function:	Change							*
 *									*
 * Description:	Changes the displayed values to either the currently	*
 *		selected force if the widget is not null, or the active	*
 *		force if the widget is null.  The newly displayed force	*
 *		is made the active force and any new/copy operation is	*
 *		canceled.						*
 ************************************************************************/

static void Change (Widget w, XtPointer client_data, XtPointer call_data)
{
    Force		 active;
    struct force	 dummy;
    ForceDialog		 forced;
    XawListReturnStruct	*info;


    forced = (ForceDialog) client_data;


    /* Retrieve the active force from the tree if selected. */

    if (w != NULL) {
	info = (XawListReturnStruct *) call_data;
	if (info -> list_index == XAW_LIST_NONE)
	    return;

	dummy.name = info -> string;
	forced -> active = (Force) TreeSearch (forced -> tree, &dummy);
    }

    active = forced -> active;
    forced -> new_copy = False;


    /* Update all of the text entries. */

    SetTextString (forced -> name, active -> name);
    LoadForceValues (forced);
}


/************************************************************************
 * Function:	Accept							*
 *									*
 * Description:	Accepts changes made to the currently displayed force.	*
 *		If the name is empty or a duplicate name is given then	*
 *		an error is reported.  Otherwise, a new force is	*
 *		created if a new/copy operation is in effect.  The	*
 *		force is then redisplayed to correct any invalid	*
 *		entries.						*
 ************************************************************************/

static void Accept (Widget w, XtPointer client_data, XtPointer call_data)
{
    struct force     old;
    struct force     dummy;
    String	     text;
    Force	     found;
    Force	     active;
    Boolean	     duplicate;
    ForceDialog	     forced;
    ForceDialogInfo  info;
    void	    (*Assign) (Force, DOF, Code, char*);
    Arg		     args [1];
    Boolean	     state;


    forced = (ForceDialog) client_data;


    /* Retrieve the name of the force. */

    dummy.name = GetTextString (forced -> name);
    found = (Force) TreeSearch (forced -> tree, &dummy);
    duplicate = found && (found != forced -> active || forced -> new_copy);

    /* Figure out which values to assign: forces or spectra */

    XtSetArg (args [0], XtNstate, &state);
    XtGetValues (forced -> time_domain, args, 1);
 
    if (state)
       Assign = AssignForce;
    else
       Assign = AssignSpectrum;


    /* Check for a duplicate name. */

    if (!dummy.name [0] || duplicate) {
	XBell (XtDisplay (forced -> name), 0);
	SetFocus (forced -> name);
	if (!forced -> new_copy)
	    SetTextString (forced -> name, forced -> active -> name);
	else
	    SetTextString (forced -> name, "");

    } else {


	/* Create a new force or new name as needed. */

	if (forced -> new_copy)
	    forced -> active = CreateForce (XtNewString (dummy.name));
	else if (strcmp (forced -> active -> name, dummy.name)) {
            old.name = forced -> active -> name;
            TreeDelete (forced -> tree, &old);
            XtFree (forced -> active -> name);
            forced -> active -> name = XtNewString (dummy.name);
            TreeInsert (forced -> tree, forced -> active);
	}

	active = forced -> active;

        if (!CompileCode (text = GetTextString (forced -> force_x)))
           Assign (active, Fx, InCore, text);
        else
           Assign (active, Fx, NULL, NULL);

        if (!CompileCode (text = GetTextString (forced -> force_y)))
           Assign (active, Fy, InCore, text);
        else
           Assign (active, Fy, NULL, NULL);

        if (!CompileCode (text = GetTextString (forced -> force_z)))
           Assign (active, Fz, InCore, text);
        else
           Assign (active, Fz, NULL, NULL);

        if (!CompileCode (text = GetTextString (forced -> moment_x)))
           Assign (active, Mx, InCore, text);
        else
           Assign (active, Mx, NULL, NULL);

        if (!CompileCode (text = GetTextString (forced -> moment_y)))
           Assign (active, My, InCore, text);
        else
           Assign (active, My, NULL, NULL);

        if (!CompileCode (text = GetTextString (forced -> moment_z)))
           Assign (active, Mz, InCore, text);
        else
           Assign (active, Mz, NULL, NULL);

	if (forced -> new_copy)
	    TreeInsert (forced -> tree, forced -> active);

	ForceDialogUpdate (forced, forced -> tree);

	if (forced -> callback != NULL) {
	    w = forced -> shell;
	    info.dialog  = forced;
	    info.force   = forced -> active;
	    info.deleted = False;
	    info.proceed = True;
	    forced -> callback (w, forced -> closure, &info);
	}
    }
}


/************************************************************************
 * Function:	Dismiss							*
 *									*
 * Description:	Pops down the dialog box.				*
 ************************************************************************/

static void Dismiss (Widget w, XtPointer client_data, XtPointer call_data)
{
    ForceDialog forced;


    forced = (ForceDialog) client_data;
    XtPopdown (forced -> shell);
}


/************************************************************************
 * Function:	Delete							*
 *									*
 * Description:	Deletes the active force if a new/copy operation is not	*
 *		in effect.  The dialog is then updated.			*
 ************************************************************************/

static void Delete (Widget w, XtPointer client_data, XtPointer call_data)
{
    ForceDialog	    forced;
    ForceDialogInfo info;


    forced = (ForceDialog) client_data;

    if (!forced -> new_copy) {
	if (forced -> callback) {
	    w = forced -> shell;
	    info.dialog  = forced;
	    info.force   = forced -> active;
	    info.deleted = True;
	    info.proceed = True;
	    forced -> callback (w, forced -> closure, &info);
	    if (info.proceed == False)
		return;
	}

	TreeDelete (forced -> tree, forced -> active);
	DestroyForce (forced -> active);
	forced -> active = NULL;
    }

    ForceDialogUpdate (forced, forced -> tree);
}

/************************************************************************
 * Function:	Toggle							*
 *									*
 * Description:								*
 ************************************************************************/

static void Toggle (Widget w, XtPointer client_data, XtPointer call_data)
{
    ForceDialog		forced;
    Boolean		state;
    Arg			args [1];

    forced = (ForceDialog) client_data;

    XtSetArg (args [0], XtNstate, &state);
    XtGetValues (w, args, 1);

    if (state) {
       XtSetArg (args [0], XtNstate, False);
       if (w == forced -> time_domain)
          XtSetValues (forced -> freq_domain, args, 1);
       else
          XtSetValues (forced -> time_domain, args, 1);
    }
    else {
       XtSetArg (args [0], XtNstate, True);
       XtSetValues (w, args, 1);
    }

    LoadForceValues (forced);
} 

/************************************************************************
 * Function:	Plot 							*
 *									*
 * Description:								*
 ************************************************************************/

static void Plot (Widget w, XtPointer client_data, XtPointer call_data)
{
    ForceDialog		forced;

    forced = (ForceDialog) client_data;

    if (w == forced -> plot_force) 
       VelvetPlotForce (forced -> active, "force");
    else
       VelvetPlotForce (forced -> active, "moment");

    return;
}

/************************************************************************
 * Function:	Copy							*
 *									*
 * Description:	Clears the name entry only and sets the flag indicating	*
 *		that a new/copy operation is in effect.			*
 ************************************************************************/

static void Copy (Widget w, XtPointer client_data, XtPointer call_data)
{
    ForceDialog forced;


    forced = (ForceDialog) client_data;

    forced -> new_copy = True;
    SetFocus (forced -> name);
    XawListUnhighlight (forced -> list);
    SetTextString (forced -> name, "");
}


/************************************************************************
 * Function:	New							*
 *									*
 * Description:	Clears all entries in the force dialog and sets the	*
 *		flag indicating that a new/copy operation is in effect.	*
 ************************************************************************/

static void New (Widget w, XtPointer client_data, XtPointer call_data)
{
    ForceDialog forced;


    forced = (ForceDialog) client_data;

    Copy (NULL, client_data, NULL);
    SetTextString (forced -> force_x, "");
    SetTextString (forced -> force_y, "");
    SetTextString (forced -> force_z, "");
    SetTextString (forced -> moment_x, "");
    SetTextString (forced -> moment_y, "");
    SetTextString (forced -> moment_z, "");
}


/************************************************************************
 * Function:	ForceDialogCreate					*
 *									*
 * Description:	Creates a new force dialog.  You would never want to	*
 *		have more than one of these but the interface is kept	*
 *		consistent with those of the other dialogs.		*
 ************************************************************************/

ForceDialog ForceDialogCreate (Widget parent, String name, String title, XtCallbackProc callback, XtPointer closure)
{
    Cardinal		i;
    Arg			args [1];
    Widget		group [18];
    ForceDialog		forced;
    Dimension		width;
    Position		x;
    static XtAppContext	app_context = NULL;
    static XtActionsRec	actions [ ] = {{"ForceDialogAction", Action}};


    /* Perform one time initialization. */

    if (app_context == NULL) {
	app_context = XtWidgetToApplicationContext (parent);
	XtAppAddActions (app_context, actions, XtNumber (actions));
	AddAutoRepeatAction (app_context);

        layout_args [0].value = StringToLayout (parent, layout_string);

	text_translations = XtParseTranslationTable (text_table);
	toggle_translations = XtParseTranslationTable (toggle_table);
	command_translations = XtParseTranslationTable (command_table);
	viewport_translations = XtParseTranslationTable (viewport_table);
	help_translations = XtParseTranslationTable (help_table);
    }


    /* Create the force dialog and its widgets. */

    XtSetArg (shell_args [0], XtNtitle, title);
    XtSetArg (shell_args [1], XtNiconName, title);

    forced = XtNew (struct force_dialog);

    forced -> callback = callback;

    forced -> closure  = closure;

    forced -> forces   = NULL;

    forced -> active   = NULL;

    forced -> shell    = XtCreatePopupShell (name,
			 topLevelShellWidgetClass, parent,
			 shell_args, XtNumber (shell_args));

    forced -> layout   = XtCreateManagedWidget ("layout",
			 layoutWidgetClass, forced -> shell,
			 layout_args, XtNumber (layout_args));

    forced -> viewport = XtCreateManagedWidget ("viewport",
			 viewportWidgetClass, forced -> layout,
			 viewport_args, XtNumber (viewport_args));

    forced -> list     = XtCreateManagedWidget ("list",
			 listWidgetClass, forced -> viewport,
			 list_args, XtNumber (list_args));

    forced -> name     = XtCreateManagedWidget ("name",
			 asciiTextWidgetClass, forced -> layout,
			 text_args, XtNumber (text_args));

    forced -> force_x  = XtCreateManagedWidget ("forceX",
			 asciiTextWidgetClass, forced -> layout,
			 text_args, XtNumber (text_args));

    forced -> force_y  = XtCreateManagedWidget ("forceY",
			 asciiTextWidgetClass, forced -> layout,
			 text_args, XtNumber (text_args));

    forced -> force_z  = XtCreateManagedWidget ("forceZ",
			 asciiTextWidgetClass, forced -> layout,
			 text_args, XtNumber (text_args));

    forced -> moment_x = XtCreateManagedWidget ("momentX",
			 asciiTextWidgetClass, forced -> layout,
			 text_args, XtNumber (text_args));

    forced -> moment_y = XtCreateManagedWidget ("momentY",
			 asciiTextWidgetClass, forced -> layout,
			 text_args, XtNumber (text_args));

    forced -> moment_z = XtCreateManagedWidget ("momentZ",
			 asciiTextWidgetClass, forced -> layout,
			 text_args, XtNumber (text_args));

    forced -> time_domain = XtCreateManagedWidget ("time_domain",
                            toggleWidgetClass, forced -> layout,
                            toggle_args, XtNumber (toggle_args));

    forced -> freq_domain = XtCreateManagedWidget ("freq_domain",
                            toggleWidgetClass, forced -> layout,
                            toggle_args, XtNumber (toggle_args));

    forced -> plot_force  = XtCreateManagedWidget ("plot_force",
			 commandWidgetClass, forced -> layout,
			 NULL, 0);

    forced -> plot_moment = XtCreateManagedWidget ("plot_moment",
			 commandWidgetClass, forced -> layout,
			 NULL, 0);

    forced -> accept   = XtCreateManagedWidget ("accept",
			 commandWidgetClass, forced -> layout,
			 NULL, 0);

    forced -> dismiss  = XtCreateManagedWidget ("dismiss",
			 commandWidgetClass, forced -> layout,
			 NULL, 0);

    forced -> delete   = XtCreateManagedWidget ("delete",
			 commandWidgetClass, forced -> layout,
			 NULL, 0);

    forced -> new      = XtCreateManagedWidget ("new",
			 commandWidgetClass, forced -> layout,
			 NULL, 0);

    forced -> copy     = XtCreateManagedWidget ("copy",
			 commandWidgetClass, forced -> layout,
			 NULL, 0);

    forced -> help     = CreateHelpButton (forced -> layout, "help");

    for (i = 0; i < XtNumber (labels); i ++) {
	label_args [0].value = (XtArgVal) labels [i];
	XtCreateManagedWidget (names [i], labelWidgetClass,
		forced -> layout, label_args, XtNumber (label_args));
    }


    XtCreateManagedWidget ("separator1", coreWidgetClass,
			forced -> layout, core_args, XtNumber (core_args));
    XtCreateManagedWidget ("separator2", coreWidgetClass,
			forced -> layout, core_args, XtNumber (core_args));
    XtCreateManagedWidget ("separator3", coreWidgetClass,
			forced -> layout, core_args, XtNumber (core_args));
    XtCreateManagedWidget ("separator4", coreWidgetClass,
			forced -> layout, core_args, XtNumber (core_args));



    /* Create a tab group for the force dialog. */

    i = 0;
    group [i++]  = forced -> name;
    group [i++]  = forced -> viewport;
    group [i++]  = forced -> time_domain;
    group [i++]  = forced -> freq_domain;
    group [i++]  = forced -> force_x;
    group [i++]  = forced -> force_y;
    group [i++]  = forced -> force_z;
    group [i++]  = forced -> moment_x;
    group [i++]  = forced -> moment_y;
    group [i++]  = forced -> moment_z;
    group [i++]  = forced -> plot_force;
    group [i++]  = forced -> plot_moment;
    group [i++]  = forced -> help;
    group [i++]  = forced -> accept;
    group [i++] = forced -> dismiss;
    group [i++] = forced -> delete;
    group [i++] = forced -> new;
    group [i++] = forced -> copy;

    XtGetValues (forced -> layout, color_args, XtNumber (color_args));
    CreateTabGroup (forced -> shell, group, XtNumber (group), highlight, True);
    XtRealizeWidget (forced -> shell);
    SetFocus (forced -> name);

    XtSetArg (args [0], XtNwidth, &width);
    XtGetValues (forced -> layout, args, 1);
    XtSetArg (args [0], XtNx, &x);
    XtGetValues (forced -> help, args, 1);
    UpdateHelpMessage (forced -> help, help_message, width - 2 * x);


    /* Add the translations to each widget. */

    AddDeleteWindowProtocol   (forced -> shell, "ForceDialogAction()");
    ListAddCursorTranslations (forced -> viewport);

    XtOverrideTranslations (forced -> name,	text_translations);
    XtOverrideTranslations (forced -> time_domain, toggle_translations);
    XtOverrideTranslations (forced -> freq_domain, toggle_translations);
    XtOverrideTranslations (forced -> force_x,	text_translations);
    XtOverrideTranslations (forced -> force_x,	text_translations);
    XtOverrideTranslations (forced -> force_y,	text_translations);
    XtOverrideTranslations (forced -> force_z,	text_translations);
    XtOverrideTranslations (forced -> moment_x,	text_translations);
    XtOverrideTranslations (forced -> moment_y,	text_translations);
    XtOverrideTranslations (forced -> moment_z,	text_translations);
    XtOverrideTranslations (forced -> plot_moment, command_translations);
    XtOverrideTranslations (forced -> plot_force,  command_translations);
    XtOverrideTranslations (forced -> accept,	command_translations);
    XtOverrideTranslations (forced -> dismiss,	command_translations);
    XtOverrideTranslations (forced -> delete,	command_translations);
    XtOverrideTranslations (forced -> new,	command_translations);
    XtOverrideTranslations (forced -> copy,	command_translations);
    XtOverrideTranslations (forced -> viewport,	viewport_translations);
    XtOverrideTranslations (forced -> help,	help_translations);


    /* Add the necessary callbacks. */

    XtAddCallback (forced -> list,    XtNcallback, Change,  (XtPointer) forced);
    XtAddCallback (forced -> accept,  XtNcallback, Accept,  (XtPointer) forced);
    XtAddCallback (forced -> dismiss, XtNcallback, Dismiss, (XtPointer) forced);
    XtAddCallback (forced -> delete,  XtNcallback, Delete,  (XtPointer) forced);
    XtAddCallback (forced -> new,     XtNcallback, New,     (XtPointer) forced);
    XtAddCallback (forced -> copy,    XtNcallback, Copy,    (XtPointer) forced);
    XtAddCallback (forced -> time_domain, XtNcallback, Toggle, (XtPointer) forced);
    XtAddCallback (forced -> freq_domain, XtNcallback, Toggle, (XtPointer) forced);
    XtAddCallback (forced -> plot_force, XtNcallback, Plot, (XtPointer) forced);
    XtAddCallback (forced -> plot_moment, XtNcallback, Plot, (XtPointer) forced);

    XtSetArg (args [0], XtNstate, True);
    XtSetValues (forced -> time_domain, args, 1);

    return forced;
}


/************************************************************************
 * Function:	ForceDialogPopup					*
 *									*
 * Description:	Pops up the specified force dialog.			*
 ************************************************************************/

void ForceDialogPopup (ForceDialog forced)
{
    XtPopup (forced -> shell, XtGrabNone);
}


/************************************************************************
 * Function:	ForceDialogActive					*
 *									*
 * Description:	Returns the currently active force.			*
 ************************************************************************/

Force ForceDialogActive (ForceDialog forced)
{
    return forced -> active;
}


/************************************************************************
 * Function:	ForceDialogDisplay					*
 *									*
 * Description:	Displays a specified force.				*
 ************************************************************************/

void ForceDialogDisplay (ForceDialog forced, Force force)
{
    forced -> active = force;
    ForceDialogUpdate (forced, forced -> tree);
}


/************************************************************************
 * Function:	ForceDialogUpdate					*
 *									*
 * Description:	Updates the specified force dialog with the specified	*
 *		tree.  If no active force exists, the first force is	*
 *		made active.  If no active force still exists then	*
 *		a new operation is performed.  Otherwise a change	*
 *		operation is performed to display the active values.	*
 ************************************************************************/

void ForceDialogUpdate (ForceDialog forced, Tree tree)
{
    Cardinal nbytes;


    /* Determine a new active force if necessary. */

    if (tree == NULL)
	tree = forced -> tree;

    if (forced -> active == NULL || tree != forced -> tree)
	forced -> active = (Force) TreeMinimum (tree);


    /* Construct the array of force names. */

    num_forces = 0;
    list_index = -1;
    dialog = forced;
    forced -> tree = tree;
    forced -> new_copy = False;

    nbytes = (TreeSize (forced -> tree) + 1) * sizeof (String);
    forced -> forces = (String *) XtRealloc ((char *) forced -> forces, nbytes);

    TreeSetIterator (forced -> tree, AppendForceName);
    TreeIterate (forced -> tree);
    forced -> forces [num_forces] = NULL;


    /* Update the list widget. */

    XawListChange (forced -> list, forced -> forces, 0, 0, True);

    if (list_index >= 0)
	XawListHighlight (forced -> list, list_index);


    /* Update the text entries. */

    if (forced -> active == NULL)
	New (NULL, (XtPointer) forced, NULL);
    else
	Change (NULL, (XtPointer) forced, NULL);
}
