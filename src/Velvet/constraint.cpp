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
 * File:	constraint.c						*
 *									*
 * Description:	This file contains the public and private functions and	*
 *		type definitions for the constraint dialog box.		*
 ************************************************************************/

# include <stdio.h>
# include <stdlib.h>
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
# include "Constraint.h"
# include "TabGroup.h"
# include "util.h"
# include "fe.h"
# include "objects.h"
# include "problem.h"


struct constraint_dialog {
    Widget         shell;	/* topLevelShell  <specified>	 */
    Widget         layout;	/*	Layout  layout		 */
    Widget         name;	/*	     AsciiText  name	 */
    Widget         toggle[7];	/*	     Toggle  toggle(1-6) */
    Widget	   dx [7];	/*	     AsciiText  dx(1-6)  */
    Widget	   ix [7];	/*	     AsciiText  ix(1-6)  */
    Widget	   vel [4];	/*    	     AsciiText  vel(1-3) */
    Widget         acc [4];     /*           AsciiText  acc(1-3) */
    Widget         viewport;	/*	     Viewport  viewport	 */
    Widget         list;	/*		  List  list	 */
    Widget         help;	/*	     MenuButton  help	 */
    Widget         accept;	/*	     Command  accept	 */
    Widget         dismiss;	/*	     Command  dismiss	 */
    Widget         nuke;	/*	     Command  delete	 */
    Widget         nu;		/*	     Command  new	 */
    Widget         copy;	/*	     Command  copy	 */
    XtCallbackProc callback;
    XtPointer	   closure;
    String        *constraints;
    Constraint     active;
    Boolean        new_copy;
    Tree           tree;
};

static String labels [ ] = {
    "Name:", "X:", "Y:", "Z:", "Trans BC", "Rot BC", 
    "Init Trans", "Init Rot", "Init Vel", "Init Accel", 
    "X:", "Y:", "Z:", "X:", "Y:", "Z:"
};

static String names [ ] = {
    "nameLabel", "x", "y", "z", "translation", "rotation", 
    "translation_ic", "rotation_ic", "velocity", "acceleration", 
    "x1", "y1", "z1", "x2", "y2", "z2"
};

static ConstraintDialog dialog;
static Cardinal	   	num_constraints;
static int	   	list_index;


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
	4 \
 	vertical { \
 	    4 \
	    ((height name - height translation) / 2) \
            height translation \
	    ((height name - height translation) / 2) \
	    4 \
	    ((height toggle1 - height x) / 2) \
 	    x \
	    ((height toggle1 - height x) / 2) \
	    4 \
	    ((height toggle2 - height y) / 2) \
 	    y \
	    ((height toggle2 - height y) / 2) \
	    4 \
	    ((height toggle3 - height z) / 2) \
 	    z \
	    ((height toggle4 - height z) / 2) \
 	    4 \
            (height translation_ic + 6) \
            ((height ix1 - height x2) / 2) \
            x2 \
            ((height ix1 - height x2) / 2) \
            4 \
            ((height ix2 - height y2) / 2) \
            y2 \
            ((height ix2 - height y2) / 2) \
            4 \
            ((height ix3 - height z2) / 2) \
            z2 \
            ((height ix3 - height z2) / 2) \
            4 \
            (height velocity + 6) \
            ((height vel1 - height x1) / 2) \
            x1 \
            ((height vel1 - height x1) / 2) \
            4 \
            ((height vel2 - height y1) / 2) \
            y1 \
            ((height vel2 - height y1) / 2) \
            4 \
            ((height vel3 - height z1) / 2) \
            z1 \
            ((height vel3 - height z1) / 2) \
            4 \
 	} \
 	4 \
 	vertical { \
 	    4 \
	    ((height name - height translation) / 2) \
	    horizontal { 1 <+inf> translation 1 <+inf> } \
	    ((height name - height translation) / 2) \
	    4 \
	    horizontal { toggle1 4 dx1 } \
	    4 \
	    horizontal { toggle2 4 dx2 } \
	    4 \
	    horizontal { toggle3 4 dx3 } \
	    6 \
            horizontal { 1 <+inf> translation_ic 1 <+inf> } \
            4 \
            horizontal { width toggle1 4 ix1 } \
            4 \
            horizontal { width toggle1 4 ix2 } \
            4 \
            horizontal { width toggle1 4 ix3 } \
            6 \
            horizontal { 1 <+inf> velocity 1 <+inf> } \
            4 \
            horizontal { width toggle1 4 vel1 } \
            4 \
            horizontal { width toggle1 4 vel2 } \
            4 \
            horizontal { width toggle1 4 vel3 } \
            4 \
 	} \
 	4 \
 	vertical { \
 	    4 \
	    ((height name - height rotation) / 2) \
	    horizontal { 1 <+inf> rotation 1 <+inf> } \
	    ((height name - height rotation) / 2) \
	    4 \
	    horizontal { toggle4 4 dx4 } \
	    4 \
	    horizontal { toggle5 4 dx5 } \
	    4 \
	    horizontal { toggle6 4 dx6 } \
	    6 \
            horizontal { 1 <+inf> rotation_ic 1 <+inf> } \
            4 \
            horizontal { width toggle4 4 ix4 } \
            4 \
            horizontal { width toggle4 4 ix5 } \
            4 \
            horizontal { width toggle4 4 ix6 } \
            6 \
            horizontal { 1 <+inf> acceleration 1 <+inf> } \
            4 \
            horizontal { width toggle4 4 acc1 } \
            4 \
            horizontal { width toggle4 4 acc2 } \
            4 \
            horizontal { width toggle4 4 acc3 } \
	    4 \
 	} \
 	4 \
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
    {XtNlabel,	     (XtArgVal) " "},
};

static Arg core_args [ ] = {
    {XtNwidth,  (XtArgVal) 3},
    {XtNheight, (XtArgVal) 3},
};


/* Translation tables */

static String text_table =
"<Key>Return: ConstraintDialogAction(accept)\n\
 <Key>Escape: ConstraintDialogAction(dismiss)\n\
 Ctrl<Key>d:  ConstraintDialogAction(delete)\n\
 Ctrl<Key>c:  ConstraintDialogAction(copy)\n\
 Ctrl<Key>n:  ConstraintDialogAction(new)\n\
 Ctrl<Key>h:  ConstraintDialogAction(help)\n\
 <Btn1Down>:  SetFocus() select-start()";

static XtTranslations text_translations;


static String command_table =
"<Key>Return:  ConstraintDialogAction(accept)\n\
 <Key>Escape:  ConstraintDialogAction(dismiss)\n\
 Ctrl<Key>d:   ConstraintDialogAction(delete)\n\
 Ctrl<Key>c:   ConstraintDialogAction(copy)\n\
 Ctrl<Key>n:   ConstraintDialogAction(new)\n\
 Ctrl<Key>h:   ConstraintDialogAction(help)\n\
 <Key>space:   AutoRepeat(off) set()\n\
 <KeyUp>space: AutoRepeat(saved) notify() unset()";

static XtTranslations command_translations;


static String toggle_table = 
"<Key>Return: ConstraintDialogAction(accept)\n\
 <Key>Escape: ConstraintDialogAction(dismiss)\n\
 Ctrl<Key>d:  ConstraintDialogAction(delete)\n\
 Ctrl<Key>c:  ConstraintDialogAction(copy)\n\
 Ctrl<Key>n:  ConstraintDialogAction(new)\n\
 Ctrl<Key>h:  ConstraintDialogAction(help)\n\
 <Key>space:  toggle() notify()";

static XtTranslations toggle_translations;


static String viewport_table =
"<Key>Return: ConstraintDialogAction(accept)\n\
 <Key>Escape: ConstraintDialogAction(dismiss)\n\
 Ctrl<Key>d:  ConstraintDialogAction(delete)\n\
 Ctrl<Key>c:  ConstraintDialogAction(copy)\n\
 Ctrl<Key>n:  ConstraintDialogAction(new)\n\
 Ctrl<Key>h:  ConstraintDialogAction(help)\n\
 <Btn1Down>:  SetFocus()";

static XtTranslations viewport_translations;


static String help_table =
"<Key>Return: ConstraintDialogAction(accept)\n\
 <Key>Escape: ConstraintDialogAction(dismiss)\n\
 Ctrl<Key>d:  ConstraintDialogAction(delete)\n\
 Ctrl<Key>c:  ConstraintDialogAction(copy)\n\
 Ctrl<Key>n:  ConstraintDialogAction(new)\n\
 Ctrl<Key>h:  ConstraintDialogAction(help)\n\
 <Key>space:  PostMenu()";

static XtTranslations help_translations;


/* Help message. */

static String help_message ="\
The constraint form is used to define, edit and delete constraints.  The name \
box displays the name of the current constraint and can be used to name (or \
rename) a constraint.  Use the list to change the current constraint.  The \
top six toggle button/text entry pairs define the boundary conditions for \
this constraint.  The buttons to the right define which DOFs are constrained. \
 If a DOF is checked, then the corresponding entry box will also be used.  \
If it is blank or 0.0 a fixed condition will be assumed.  If it contains a \
number, this DOF will be treated as a displacement BC.  You can also put the \
keyword 'hinge' in these boxes to define a hinged BC or a time-varying \
expression to define a time-dependent BC in a transient analysis problem.  \
The initial displacement, velocity, and acceleration entries are used to \
define initial conditions in transient analysis problems.  Use the 'Accept' \
button to register your changes.  'Delete' erases the current constraint.  \
'New' empties all fields.  'Copy' empties the name field only.";


/************************************************************************
 * Function:	ToggleButtonSetState					*
 *									*
 * Description:	Sets the state of the specified toggle button.		*
 ************************************************************************/

static void ToggleButtonSetState (Widget w, Boolean state)
{
   Arg		arglist [1]; 


   XtSetArg (arglist [0], XtNstate, state);
   XtSetValues (w, arglist, 1);
}


/************************************************************************
 * Function:	ToggleButtonGetState					*
 *									*
 * Description:	Retrieves the state of the specified toggle button.	*
 ************************************************************************/

static Boolean ToggleButtonGetState (Widget w)
{
   Arg		arglist [1];
   Boolean	state;


   XtSetArg (arglist [0], XtNstate, &state);
   XtGetValues (w, arglist, 1);

   return state;
}


/************************************************************************
 * Function:	SetSensitive						*
 *									*
 * Description:	Works just like XtSetSensitive but moves the keyboard	*
 *		focus to the specified widget if necessary.		*
 ************************************************************************/

static void SetSensitive (Widget w, Boolean value, Widget move_to)
{
    if (value == False && HasFocus (w) == True)
	SetFocus (move_to);

    if (value == False)
	SetTextString (w, "");

    XtSetSensitive (w, value);
}


/************************************************************************
 * Function:	Toggle							*
 *									*
 * Description:	Adjusts the sensitivity of the corresponding text	*
 *		entry when a toggle widget is toggled.			*
 ************************************************************************/

static void Toggle (Widget w, XtPointer client_data, XtPointer call_data)
{
    Widget text;


    text = (Widget) client_data;

    SetSensitive (text, ToggleButtonGetState (w), w);
}


/************************************************************************
 * Function:	AppendConstraintName					*
 *									*
 * Description:	Appends the constraint name to the array of names.  The	*
 *		index of the active constraint is also set.		*
 ************************************************************************/

static int AppendConstraintName (Item item)
{
    if (dialog -> active == (Constraint) item)
	list_index = num_constraints;

    dialog -> constraints [num_constraints ++] = XtNewString(((Constraint) item) -> name.c_str());
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


/************************************************************************
 * Function:	Change							*
 *									*
 * Description:	Changes the displayed values to either the currently	*
 *		selected constraint if the widget is not null, or the 	*
 *		activeconstraint if the widget is null.  The newly 	*
 *		displayed constraint is made the active constraint and  * 
 *		any new/copy operation is canceled.			*
 ************************************************************************/

static void Change (Widget w, XtPointer client_data, XtPointer call_data)
{
    char		 buffer [32];
    Constraint		 active;
    struct constraint	 dummy;
    ConstraintDialog	 constraintd;
    XawListReturnStruct	*info;
    unsigned		 i;


    constraintd = (ConstraintDialog) client_data;


    /* Retrieve the active constraint from the tree if selected. */

    if (w != NULL) {
	info = (XawListReturnStruct *) call_data;
	if (info -> list_index == XAW_LIST_NONE)
	    return;

	dummy.name = info -> string;
	constraintd -> active = (Constraint) TreeSearch (constraintd -> tree, 
                                                         &dummy);
    }

    active = constraintd -> active;
    constraintd -> new_copy = False;


    /* Update the name. */

    SetTextString (constraintd -> name, active -> name.c_str());


    /* Update the toggle buttons. */

    for (i = 1 ; i <= 6 ; i++)
        ToggleButtonSetState (constraintd -> toggle [i], active -> constraint[i] != 0);


    /* Update the text entries. */

    for (i = 1 ; i <= 3 ; i++) {
        if (active -> constraint [i] == 1) {
	   SetSensitive (constraintd -> dx [i], True, NULL);
           if (active -> dx [i].expr)
              sprintf (buffer, "%s", active -> dx [i].text);
           else
	      sprintf (buffer, (active -> dx [i].value ? "%g" : ""), active -> dx [i].value);
	   SetTextString (constraintd -> dx [i], buffer);
        } else {
	   SetTextString (constraintd -> dx [i], "");
	   SetSensitive (constraintd -> dx [i], False, constraintd -> toggle [i]);
        }
    }

    for (i = 4 ; i <= 6 ; i++) {
        if (active -> constraint [i] == 1) {
	   SetSensitive (constraintd -> dx [i], True, NULL);
           if (active -> dx [i].expr)
              sprintf (buffer, "%s", active -> dx [i].text);
           else
   	      sprintf (buffer, (active -> dx [i].value ? "%g" : ""), active -> dx [i].value);
	   SetTextString (constraintd -> dx [i], buffer);
        } else if (active -> constraint [i] == 'h') {
    	   SetSensitive (constraintd -> dx [i], True, NULL);
 	   SetTextString (constraintd -> dx [i], "hinge");
        } else {
	   SetTextString (constraintd -> dx [i], "");
	   SetSensitive (constraintd -> dx [i], False, constraintd -> toggle [i]);
        }
    }

    for (i = 1 ; i <= 6 ; i++) {
        if (active -> ix [i]) {
           sprintf (buffer,"%g", active -> ix [i]);
           SetTextString (constraintd -> ix [i], buffer);
        } else
           SetTextString (constraintd -> ix [i], "");
    }

    for (i = 1 ; i <= 3 ; i++) {
        if (active -> vx [i]) {
           sprintf (buffer,"%g", active -> vx [i]);
           SetTextString (constraintd -> vel [i], buffer);
        } else
           SetTextString (constraintd -> vel [i], "");

        if (active -> ax [i] != UnspecifiedValue) {
           sprintf (buffer,"%g", active -> ax [i]);
           SetTextString (constraintd -> acc [i], buffer);
        } else
           SetTextString (constraintd -> acc [i], "");
    }
}


/****************************************************************************
 * Function:	Accept						   	    *
 *									    *
 * Description:	Accepts changes made to the currently displayed constraint. *
 *		If the name is empty or a duplicate name is given then	    *
 *		an error is reported.  Otherwise, a new constraint is	    *
 *		created if a new/copy operation is in effect.  The	    *
 *		constraint is then redisplayed to correct any invalid	    *
 *		entries.						    *
 ****************************************************************************/

static void Accept (Widget w, XtPointer client_data, XtPointer call_data)
{
    char		*ptr;
    String		 value;
    double		 accel;
    struct constraint	 old;
    struct constraint    dummy;
    Constraint	 	 found;
    Constraint	 	 active;
    Boolean	 	 duplicate;
    ConstraintDialog	 constraintd;
    ConstraintDialogInfo info;
    unsigned		 i;


    constraintd = (ConstraintDialog) client_data;


    /* Retrieve the name of the constraint. */

    dummy.name = GetTextString (constraintd -> name);
    found = (Constraint) TreeSearch (constraintd -> tree, &dummy);
    duplicate = found && (found != constraintd -> active || 
                                   constraintd -> new_copy);


    /* Check for a duplicate name. */

    if (!dummy.name [0] || duplicate) {
	XBell (XtDisplay (constraintd -> name), 0);
	SetFocus (constraintd -> name);
	if (!constraintd -> new_copy)
	    SetTextString (constraintd -> name, constraintd -> active -> name.c_str());
	else
	    SetTextString (constraintd -> name, "");

    } else {
        

	/* Create a new constraint or new name as needed. */
       
	if (constraintd -> new_copy)
	    constraintd -> active = CreateConstraint (dummy.name.c_str());
	else if (strcmp (constraintd -> active -> name.c_str(), dummy.name.c_str())) {
            old.name = constraintd -> active -> name;
            TreeDelete (constraintd -> tree, &old);
            constraintd -> active -> name = dummy.name;
            TreeInsert (constraintd -> tree, constraintd -> active);
	}
        
	active = constraintd -> active;
  
        for (i = 1 ; i <= 6 ; i++)
	   active -> constraint[i] = ToggleButtonGetState(constraintd -> toggle [i]);


        for (i = 1 ; i <= 3 ; i++) {
	   if (active -> constraint [i]) {
	       value = GetTextString (constraintd -> dx[i]);
               if (!CompileCode (value))
                   AssignConstraint (active, (DOF) i, InCore, value, 1);
               else
                   AssignConstraint (active, (DOF) i, NULL, NULL, 1);
           }
        }

        for (i = 4 ; i <= 6 ; i++) {
	   if (active -> constraint [i]) {
	      value = GetTextString (constraintd -> dx[i]);
	      if (!strcmp (value, "h") || !strcmp (value, "hinge"))
              AssignConstraint (active, (DOF) i, NULL, NULL, 'h'); 
	      else {
                  if (!CompileCode (value))
                      AssignConstraint (active, (DOF) i, InCore, value, 1);
                  else
                      AssignConstraint (active, (DOF) i, NULL, NULL, 1); 
              }
	   }
        }

        for (i = 1 ; i <= 3 ; i++) {
           value = GetTextString (constraintd -> acc [i]);
           accel = exptod (value, &ptr);
           if (*ptr != 0)
              active -> ax [i] = UnspecifiedValue;
           else
              active -> ax [i] = accel;

           value = GetTextString (constraintd -> vel [i]);
           active -> vx [i] = exptod (value, NULL);
        }

        for (i = 1 ; i <= 6 ; i++) {
           value = GetTextString (constraintd -> ix [i]);
           active -> ix [i] = exptod (value, NULL);
        }

	if (constraintd -> new_copy)
	    TreeInsert (constraintd -> tree, constraintd -> active);

	if (constraintd -> callback != NULL) {
	    w = constraintd -> shell;
	    info.dialog     = constraintd;
	    info.constraint = constraintd -> active;
	    info.deleted    = False;
	    info.proceed    = True;
	    constraintd -> callback (w, constraintd -> closure, &info);
	}

	ConstraintDialogUpdate (constraintd, constraintd -> tree);
    }
}


/************************************************************************
 * Function:	Dismiss							*
 *									*
 * Description:	Pops down the dialog box.				*
 ************************************************************************/

static void Dismiss (Widget w, XtPointer client_data, XtPointer call_data)
{
    ConstraintDialog constraintd;


    constraintd = (ConstraintDialog) client_data;
    XtPopdown (constraintd -> shell);
}


/************************************************************************
 * Function:	Delete							*
 *									*
 * Description:	Deletes the active constraint if a new/copy operation   *
 *		is not in effect.  The dialog is then updated.	 	*	
 ************************************************************************/

static void Delete (Widget w, XtPointer client_data, XtPointer call_data)
{
    ConstraintDialog     constraintd;
    ConstraintDialogInfo info;


    constraintd = (ConstraintDialog) client_data;

    if (!constraintd -> new_copy) {
	if (constraintd -> callback != NULL) {
	    w = constraintd -> shell;
	    info.dialog     = constraintd;
	    info.constraint = constraintd -> active;
	    info.deleted    = True;
	    info.proceed    = True;
	    constraintd -> callback (w, constraintd -> closure, &info);
	    if (info.proceed == False)
		return;
	}

	TreeDelete (constraintd -> tree, constraintd -> active);
	DestroyConstraint (constraintd -> active);
	constraintd -> active = NULL;
    }

    ConstraintDialogUpdate (constraintd, constraintd -> tree);
}


/************************************************************************
 * Function:	Copy							*
 *									*
 * Description:	Clears the name entry only and sets the flag indicating	*
 *		that a new/copy operation is in effect.			*
 ************************************************************************/

static void Copy (Widget w, XtPointer client_data, XtPointer call_data)
{
    ConstraintDialog constraintd;


    constraintd = (ConstraintDialog) client_data;

    constraintd -> new_copy = True;
    SetFocus (constraintd -> name);
    XawListUnhighlight (constraintd -> list);
    SetTextString (constraintd -> name, "");
}


/************************************************************************
 * Function:	New							*
 *									*
 * Description:	Clears all entries in the constraint dialog; sets the	*
 *		flag indicating that a new/copy operation is in effect.	*
 ************************************************************************/

static void New (Widget w, XtPointer client_data, XtPointer call_data)
{
    ConstraintDialog constraintd;
    unsigned	     i;


    constraintd = (ConstraintDialog) client_data;

    Copy (NULL, client_data, NULL);

    for (i = 1 ; i <= 6 ; i++) {
       ToggleButtonSetState (constraintd -> toggle [i], False);
       SetTextString (constraintd -> dx [i], "");
       SetTextString (constraintd -> ix [i], "");
       SetSensitive (constraintd -> dx [i], False, constraintd -> toggle [i]);
       if (i <= 3) {
          SetTextString (constraintd -> vel [i], "");
          SetTextString (constraintd -> acc [i], "");
       }
    }
}


/************************************************************************
 * Function:	ConstraintDialogCreate					*
 *									*
 * Description:	Creates a new constraint dialog.  You never want to	*
 *		have more than one of these but the interface is kept	*
 *		consistent with those of the other dialogs.		*
 ************************************************************************/

ConstraintDialog ConstraintDialogCreate (Widget parent, String name, String title, XtCallbackProc callback, XtPointer closure)
{
    Cardinal		i, j;
    char		buffer [32];
    Arg			args [1];
    Widget		group [32];
    ConstraintDialog	constraintd;
    Dimension		width;
    Position		x;
    static XtAppContext	app_context = NULL;
    static XtActionsRec	actions [ ] = {{"ConstraintDialogAction", Action}};


    /* Perform one time initialization. */

    if (app_context == NULL) {
	app_context = XtWidgetToApplicationContext (parent);
	XtAppAddActions (app_context, actions, XtNumber (actions));
	AddAutoRepeatAction (app_context);

        layout_args [0].value = StringToLayout (parent, layout_string);

	text_translations = XtParseTranslationTable (text_table);
	command_translations = XtParseTranslationTable (command_table);
        toggle_translations = XtParseTranslationTable (toggle_table);
	viewport_translations = XtParseTranslationTable (viewport_table);
	help_translations = XtParseTranslationTable (help_table);
    }


    /* Create the constraint dialog and its widgets. */

    XtSetArg (shell_args [0], XtNtitle, title);
    XtSetArg (shell_args [1], XtNiconName, title);

    constraintd = XtNew (struct constraint_dialog);

    constraintd -> callback = callback;

    constraintd -> closure = closure;

    constraintd -> constraints   = NULL;

    constraintd -> active   = NULL;

    constraintd -> shell  = XtCreatePopupShell (name,
			 topLevelShellWidgetClass, parent,
			 shell_args, XtNumber (shell_args));

    constraintd -> layout   = XtCreateManagedWidget ("layout",
			 layoutWidgetClass, constraintd -> shell,
			 layout_args, XtNumber (layout_args));

    constraintd -> viewport = XtCreateManagedWidget ("viewport",
			 viewportWidgetClass, constraintd -> layout,
			 viewport_args, XtNumber (viewport_args));

    constraintd -> list     = XtCreateManagedWidget ("list",
			 listWidgetClass, constraintd -> viewport,
			 list_args, XtNumber (list_args));

    constraintd -> name     = XtCreateManagedWidget ("name",
			 asciiTextWidgetClass, constraintd -> layout,
			 text_args, XtNumber (text_args));

    for (i = 1 ; i <= 6 ; i++) {
       sprintf (buffer, "toggle%d", i);
       constraintd -> toggle[i]  = XtCreateManagedWidget (buffer,
   			             toggleWidgetClass, constraintd -> layout,
			             toggle_args, XtNumber (toggle_args));

       sprintf (buffer, "dx%d", i);
       constraintd -> dx[i] = XtCreateManagedWidget (buffer,
                		   asciiTextWidgetClass, constraintd -> layout,
			           text_args, XtNumber (text_args));

       sprintf (buffer, "ix%d", i);
       constraintd -> ix[i] = XtCreateManagedWidget (buffer,
  			         asciiTextWidgetClass, constraintd -> layout,
			         text_args, XtNumber (text_args));
    }

    for (i = 1 ; i <= 3 ; i++) {
       sprintf (buffer, "vel%d", i);
       constraintd -> vel[i]  = XtCreateManagedWidget (buffer,
                               asciiTextWidgetClass, constraintd -> layout,
                               text_args, XtNumber (text_args));

       sprintf (buffer, "acc%d", i);
       constraintd -> acc[i]  = XtCreateManagedWidget (buffer,
                               asciiTextWidgetClass, constraintd -> layout,
                               text_args, XtNumber (text_args));
    }

    constraintd -> accept   = XtCreateManagedWidget ("accept",
			 commandWidgetClass, constraintd -> layout,
			 NULL, 0);

    constraintd -> dismiss  = XtCreateManagedWidget ("dismiss",
			 commandWidgetClass, constraintd -> layout,
			 NULL, 0);

    constraintd -> nuke   = XtCreateManagedWidget ("delete",
			 commandWidgetClass, constraintd -> layout,
			 NULL, 0);

    constraintd -> nu      = XtCreateManagedWidget ("new",
			 commandWidgetClass, constraintd -> layout,
			 NULL, 0);

    constraintd -> copy     = XtCreateManagedWidget ("copy",
			 commandWidgetClass, constraintd -> layout,
			 NULL, 0);

    constraintd -> help     = CreateHelpButton (constraintd -> layout, "help");

    for (i = 0; i < XtNumber (labels); i ++) {
	label_args [0].value = (XtArgVal) labels [i];
	XtCreateManagedWidget (names [i], labelWidgetClass,
		constraintd -> layout, label_args, XtNumber (label_args));
    }


    XtCreateManagedWidget ("separator1", coreWidgetClass,
			constraintd -> layout, core_args, XtNumber (core_args));

    XtCreateManagedWidget ("separator2", coreWidgetClass,
			constraintd -> layout, core_args, XtNumber (core_args));


    /* Create a tab group for the constraint dialog. */

    i = 0;
    group [i++]  = constraintd -> name;
    group [i++]  = constraintd -> viewport;
    for (j = 1 ; j <= 6 ; j++) {
       group [i++]  = constraintd -> toggle[j];
       group [i++]  = constraintd -> dx[j];
    }
    for (j = 1 ; j <= 6 ; j++) 
       group [i++] = constraintd -> ix[j];
    
    group [i++] = constraintd -> vel[1];
    group [i++] = constraintd -> vel[2];
    group [i++] = constraintd -> vel[3];
    group [i++] = constraintd -> acc[1];
    group [i++] = constraintd -> acc[2];
    group [i++] = constraintd -> acc[3];

    group [i++] = constraintd -> help;
    group [i++] = constraintd -> accept;
    group [i++] = constraintd -> dismiss;
    group [i++] = constraintd -> nuke;
    group [i++] = constraintd -> nu;
    group [i++] = constraintd -> copy;

    XtGetValues (constraintd -> layout, color_args, XtNumber (color_args));
    CreateTabGroup (constraintd -> shell, group, XtNumber (group), 
                    highlight, True);

    XtRealizeWidget (constraintd -> shell);
    SetFocus (constraintd -> name);

    XtSetArg (args [0], XtNwidth, &width);
    XtGetValues (constraintd -> layout, args, 1);
    XtSetArg (args [0], XtNx, &x);
    XtGetValues (constraintd -> help, args, 1);
    UpdateHelpMessage (constraintd -> help, help_message, width - 2 * x);


    /* Add the translations to each widget. */

    AddDeleteWindowProtocol (constraintd -> shell, "ConstraintDialogAction()");
    ListAddCursorTranslations (constraintd -> viewport);

    XtOverrideTranslations (constraintd -> name,	text_translations);

    for (i = 1 ; i <= 6 ; i++) {
       XtOverrideTranslations (constraintd -> dx[i],	 text_translations);
       XtOverrideTranslations (constraintd -> ix[i],	 text_translations);
       XtOverrideTranslations (constraintd -> toggle[i], toggle_translations);
       XtAddCallback (constraintd -> toggle[i], XtNcallback,
   		      Toggle,  (XtPointer) constraintd -> dx[i]);
    }
    for (i = 1 ; i <= 3 ; i++) {
       XtOverrideTranslations (constraintd -> vel[i],	text_translations);
       XtOverrideTranslations (constraintd -> acc[i],	text_translations);
    }

    XtOverrideTranslations (constraintd -> accept,	command_translations);
    XtOverrideTranslations (constraintd -> dismiss,	command_translations);
    XtOverrideTranslations (constraintd -> nuke,	command_translations);
    XtOverrideTranslations (constraintd -> nu,	        command_translations);
    XtOverrideTranslations (constraintd -> copy,	command_translations);
    XtOverrideTranslations (constraintd -> viewport,	viewport_translations);
    XtOverrideTranslations (constraintd -> help,	help_translations);


    /* Add the necessary callbacks. */

    XtAddCallback (constraintd -> list,    XtNcallback, 
                   Change,  (XtPointer) constraintd);
    XtAddCallback (constraintd -> accept,  XtNcallback, 
                   Accept,  (XtPointer) constraintd);
    XtAddCallback (constraintd -> dismiss, XtNcallback, 
                   Dismiss, (XtPointer) constraintd);
    XtAddCallback (constraintd -> nuke,  XtNcallback, 
                   Delete,  (XtPointer) constraintd);
    XtAddCallback (constraintd -> nu,     XtNcallback, 
                   New,     (XtPointer) constraintd);
    XtAddCallback (constraintd -> copy,    XtNcallback, 
                   Copy,    (XtPointer) constraintd);

    return constraintd;
}


/************************************************************************
 * Function:	ConstraintDialogPopup					*
 *									*
 * Description:	Pops up the specified constraint dialog.		*
 ************************************************************************/

void ConstraintDialogPopup (ConstraintDialog constraintd)
{
    XtPopup (constraintd -> shell, XtGrabNone);
}


/************************************************************************
 * Function:	ConstraintDialogActive					*
 *									*
 * Description:	Returns the currently active constraint.		*
 ************************************************************************/

Constraint ConstraintDialogActive (ConstraintDialog constraintd)
{
    return constraintd -> active;
}


/************************************************************************
 * Function:	ConstraintDialogDisplay					*
 *									*
 * Description:	Displays specified constraint.				*	
 ************************************************************************/

void ConstraintDialogDisplay (ConstraintDialog constraintd, Constraint constraint)
{
    constraintd -> active = constraint;
    ConstraintDialogUpdate (constraintd, constraintd -> tree);
}


/************************************************************************
 * Function:	ConstraintDialogUpdate					*
 *									*
 * Description:	Updates the specified constraint dialog with the 	*
 *		specified tree.  If no active constraint exists, the 	*
 *		first constraint is made active.  If no active 		*
 *		constraint still exists then a new operation is 	*
 *		performed.  Otherwise a change operation is performed 	*
 *		to display the active values.				*
 ************************************************************************/

void ConstraintDialogUpdate (ConstraintDialog constraintd, Tree tree)
{
    Cardinal nbytes;


    /* Determine a new active constraint if necessary. */

    if (tree == NULL)
	tree = constraintd -> tree;

    if (constraintd -> active == NULL || tree != constraintd -> tree)
	constraintd -> active = (Constraint) TreeMinimum (tree);


    /* Construct the array of constraint names. */

    num_constraints = 0;
    list_index = -1;
    dialog = constraintd;
    constraintd -> tree = tree;
    constraintd -> new_copy = False;

    nbytes = (TreeSize (constraintd -> tree) + 1) * sizeof (String);
    constraintd -> constraints = 
         (String *) XtRealloc ((char *) constraintd -> constraints, nbytes);

    TreeSetIterator (constraintd -> tree, AppendConstraintName);
    TreeIterate (constraintd -> tree);
    constraintd -> constraints [num_constraints] = NULL;


    /* Update the list widget. */

    XawListChange (constraintd -> list, constraintd -> constraints, 0,0,True);

    if (list_index >= 0)
	XawListHighlight (constraintd -> list, list_index);


    /* Update the text entries. */

    if (constraintd -> active == NULL)
	New (NULL, (XtPointer) constraintd, NULL);
    else
	Change (NULL, (XtPointer) constraintd, NULL);
}
