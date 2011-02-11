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
 * File:	loadcase.c						*
 *									*
 * Description:	This file contains the public and private function and	*
 *		type definitions for the loadcase dialog box.		*
 ************************************************************************/

# include <vector>
# include <string>
# include <algorithm>
# include <stdio.h>
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Shell.h>
# include <X11/Xaw/AsciiText.h>
# include <X11/Xaw/Command.h>
# include <X11/Xaw/Label.h>
# include <X11/Xaw/List.h>
# include <X11/Xaw/Viewport.h>
# include <X11/Xaw/MenuButton.h>
# include <X11/Xaw/Repeater.h>
# include <X11/Xaw/SimpleMenu.h>
# include <X11/Xaw/SmeBSB.h>
# include "Layout.h"
# include "LoadCase.h"
# include "TabGroup.h"
# include "util.h"
# include "objects.h"
# include "post.h"
# include "problem.h"
# include "allocate.h"
# include "error.h"

# ifndef X_NOT_STDC_ENV
# include <stdlib.h>
# endif


struct loadcase_dialog {
    Widget         shell;	   /* topLevelShell  <specified>	*/
    Widget         layout;	   /*	Layout  layout		   	*/
    Widget         name;	   /*	     AsciiText  name   	   	*/
    Widget         node [4];	   /*	     AsciiText  node{i}	   	*/
    Widget         element [4];	   /*	     AsciiText  element{i} 	*/
    Widget   	   force [4];	   /*	     AsciiText  force{i}	*/
    Widget   	   load [4];	   /*	     AsciiText  load{i}		*/
    Widget         force_button;   /*	     MenuButton  force_button   */
    Widget         force_menu;     /*	       SimpleMenu  force_menu	*/
    Widget         load_button;    /*	     MenuButton  load_button    */
    Widget         load_menu;      /*	       SimpleMenu  load_menu  	*/
    Widget         node_up;	   /*	     Command  node_up	   	*/
    Widget         node_down;	   /*	     Command  node_down	   	*/
    Widget	   element_up;	   /*	     Command  element_up   	*/
    Widget	   element_down;   /*	     Command  element_down 	*/
    Widget         viewport;	   /*	     Viewport viewport	   	*/
    Widget         list;	   /*	       List     list	   	*/
    Widget         help;	   /*	     MenuButton  help	   	*/
    Widget         accept;	   /*	     Command  accept	   	*/
    Widget         dismiss;	   /*	     Command  dismiss	   	*/
    Widget         nuke;	   /*	     Command  delete	   	*/
    Widget         nu;		   /*	     Command  new	   	*/
    Widget         copy;	   /*	     Command  copy	   	*/
    String	  *loadcases;
    LoadCase	   active;
    Boolean        new_copy;
    Tree           tree;
    unsigned	   node_base;
    unsigned	   element_base;
    String	   force_assignments [100];
    unsigned	   node_assignments [100];
    String	   load_assignments [100];
    unsigned       element_assignments [100];
    unsigned       num_forces;
    unsigned       num_loads;
};

static String labels [ ] = {
    "Name:", "node", "element"
};

static String names [ ] = {
    "nameLabel", "nodeLabel", "elementLabel"
};

static LoadCaseDialog  dialog;
static Cardinal	       num_loadcases;
static int	       list_index;


/* Bitmaps */

#define up_width 12
#define up_height 12
static char up_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x60, 0x00, 0xf0, 0x00, 0xf8, 0x01,
   0xfc, 0x03, 0xfe, 0x07, 0xfe, 0x07, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};

#define down_width 12
#define down_height 12
static char down_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xfe, 0x07, 0xfe, 0x07, 0xfc, 0x03,
   0xf8, 0x01, 0xf0, 0x00, 0x60, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};

static Pixmap up_bitmap;
static Pixmap down_bitmap;


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
 	12 \
	separator1 <* +inf -100%> \
        12 \
        vertical { \
            4 \
            horizontal { \
               ((width node1 - width nodeLabel) / 2) \
               nodeLabel \
               ((width node1 - width nodeLabel) / 2) \
	       16 \
               ((width force1 - width force_button) / 2) \
               force_button <+inf -100% *> \
               ((width force1 - width force_button) / 2) \
            } \
            4 \
            horizontal { \
               node1 \
               16 \
               force1 <+inf -100% *> \
            } \
            4 \
            horizontal { \
               node2 \
               16 \
               force2 <+inf -100% *> \
            } \
            4 \
            horizontal { \
               node3 \
               16 \
               force3 <+inf -100% *> \
            } \
            4 \
            horizontal { \
               node4 \
               16 \
               force4 <+inf -100% *> \
            } \
            8 \
            horizontal { \
               node_down \
               4 \
               node_up \
            } \
            4 \
        } \
        12 \
	separator3 <* +inf -100%> \
        12 \
        vertical { \
            4 \
            horizontal { \
               ((width element1 - width elementLabel) / 2) \
               elementLabel \
               ((width element1 - width elementLabel) / 2) \
	       16 \
               ((width load1 - width load_button) / 2) \
               load_button <+inf -100% *> \
               ((width load1 - width load_button) / 2) \
            } \
            4 \
            horizontal { \
               element1 \
               16 \
               load1 <+inf -100% *> \
            } \
            4 \
            horizontal { \
               element2 \
               16 \
               load2 <+inf -100% *> \
            } \
            4 \
            horizontal { \
               element3 \
               16 \
               load3 <+inf -100% *> \
            } \
            4 \
            horizontal { \
               element4 \
               16 \
               load4 <+inf -100% *> \
            } \
            8 \
            horizontal { \
               element_down \
               4 \
               element_up \
            } \
            4 \
        } \
        12 \
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

static Arg number_args [ ] = {
    {XtNeditType,    (XtArgVal) XawtextEdit},
    {XtNborderWidth, (XtArgVal) 0},
    {XtNpieceSize,   (XtArgVal) 32},
    {XtNcursorName,  (XtArgVal) "left_ptr"},
    {XtNwidth,       (XtArgVal) 40},
};

static Arg name_args [ ] = {
    {XtNeditType,    (XtArgVal) XawtextEdit},
    {XtNborderWidth, (XtArgVal) 0},
    {XtNpieceSize,   (XtArgVal) 32},
    {XtNcursorName,  (XtArgVal) "left_ptr"},
};

static Arg label_args [ ] = {
    {XtNlabel,       (XtArgVal) ""},
    {XtNborderWidth, (XtArgVal) 0},
};


static Arg core_args [ ] = {
    {XtNwidth,  (XtArgVal) 3},
    {XtNheight, (XtArgVal) 3},
};


static Arg repeater_args [ ] = {
    {XtNbitmap, (XtArgVal) NULL},
};


static Arg button_args [ ] = {
    {XtNlabel,       (XtArgVal) ""},
    {XtNmenuName,    (XtArgVal) ""},
    {XtNborderWidth, (XtArgVal) 0},
};


/* Translation tables */

static String text_table =
"<Key>Return: LoadCaseDialogAction(accept)\n\
 <Key>Escape: LoadCaseDialogAction(dismiss)\n\
 Ctrl<Key>d:  LoadCaseDialogAction(delete)\n\
 Ctrl<Key>c:  LoadCaseDialogAction(copy)\n\
 Ctrl<Key>n:  LoadCaseDialogAction(new)\n\
 Ctrl<Key>h:  LoadCaseDialogAction(help)\n\
 <Btn1Down>:  SetFocus() select-start()";

static XtTranslations text_translations;


static String button_table =
"<Key>Return: LoadCaseDialogAction(accept)\n\
 <Key>Escape: LoadCaseDialogAction(dismiss)\n\
 Ctrl<Key>d:  LoadCaseDialogAction(delete)\n\
 Ctrl<Key>c:  LoadCaseDialogAction(copy)\n\
 Ctrl<Key>n:  LoadCaseDialogAction(new)\n\
 Ctrl<Key>h:  LoadCaseDialogAction(help)\n\
 <BtnDown>:   PostMenu()\n\
 <Key>space:  PostMenu()";

static XtTranslations button_translations;


static String repeater_table =
"<Key>Return:  LoadCaseDialogAction(accept)\n\
 <Key>Escape:  LoadCaseDialogAction(dismiss)\n\
 Ctrl<Key>d:   LoadCaseDialogAction(delete)\n\
 Ctrl<Key>c:   LoadCaseDialogAction(copy)\n\
 Ctrl<Key>n:   LoadCaseDialogAction(new)\n\
 Ctrl<Key>h:   LoadCaseDialogAction(help)\n\
 <Key>space:   AutoRepeat(off) set()\n\
 <KeyUp>space: AutoRepeat(saved) notify() unset()";

static XtTranslations repeater_translations;


static String command_table =
"<Key>Return:  LoadCaseDialogAction(accept)\n\
 <Key>Escape:  LoadCaseDialogAction(dismiss)\n\
 Ctrl<Key>d:   LoadCaseDialogAction(delete)\n\
 Ctrl<Key>c:   LoadCaseDialogAction(copy)\n\
 Ctrl<Key>n:   LoadCaseDialogAction(new)\n\
 Ctrl<Key>h:   LoadCaseDialogAction(help)\n\
 <Key>space:   AutoRepeat(off) set()\n\
 <KeyUp>space: AutoRepeat(saved) notify() unset()";

static XtTranslations command_translations;


static String viewport_table =
"<Key>Return: LoadCaseDialogAction(accept)\n\
 <Key>Escape: LoadCaseDialogAction(dismiss)\n\
 Ctrl<Key>d:  LoadCaseDialogAction(delete)\n\
 Ctrl<Key>c:  LoadCaseDialogAction(copy)\n\
 Ctrl<Key>n:  LoadCaseDialogAction(new)\n\
 Ctrl<Key>h:  LoadCaseDialogAction(help)\n\
 <Btn1Down>:  SetFocus()";

static XtTranslations viewport_translations;


static String help_table =
"<Key>Return: LoadCaseDialogAction(accept)\n\
 <Key>Escape: LoadCaseDialogAction(dismiss)\n\
 Ctrl<Key>d:  LoadCaseDialogAction(delete)\n\
 Ctrl<Key>c:  LoadCaseDialogAction(copy)\n\
 Ctrl<Key>n:  LoadCaseDialogAction(new)\n\
 Ctrl<Key>h:  LoadCaseDialogAction(help)\n\
 <Key>space:  PostMenu()";

static XtTranslations help_translations;


/* Help message. */

static String help_message ="";


/************************************************************************
 * Function:	AppendLoadCaseName					*
 *									*
 * Description:	Appends the loadcase name to the array of names.  The	*
 *		index of the active loadcase is also set.		*
 ************************************************************************/

static int AppendLoadCaseName (Item item)
{
    if (dialog -> active == (LoadCase) item)
	list_index = num_loadcases;

    dialog -> loadcases [num_loadcases ++] = XtNewString(((LoadCase) item) -> name.c_str());
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
 * Function:	ScrollNode	
 *	
 * Description:	 
 ************************************************************************/

static void ScrollNode (Widget w, XtPointer client_data, XtPointer call_data)
{
    Arg		    args [1];
    LoadCaseDialog  loadcased;
    unsigned	    current [4];
    unsigned	    count;
    String	    value;
    char	    buffer [10];
    unsigned	    node;
    unsigned	    i;
    unsigned	    base;
    int     	    dir;

    loadcased = (LoadCaseDialog) client_data;

    base = loadcased -> node_base;

   	/*
	 * figure out which entries have numbers in them
	 */

    XtSetArg (args [0], XtNstring,  &value);
    
    count = 0;
    for (i = 0 ; i < 4 ; i++) {
       XtGetValues (loadcased -> node [i], args, 1);
       node = atoi (value);
       if (node != 0) 
          current [count++] = node;
    }

	/*
	 * if necessary (and allowed) actually do the shift
	 */

    if (w == loadcased -> node_up && count == 4) 
       dir = 1;
    else if (w == loadcased -> node_down && base > 0) 
       dir = -1;
    else
       return;

    fprintf (stderr,"base = %d\n", base);
    fprintf (stderr,"count = %d\n", count);
    fprintf (stderr,"num_forces = %d\n", loadcased -> num_forces);

    if (base + count > loadcased -> num_forces) 
       loadcased -> num_forces = base + count;
    else if (base + count < loadcased -> num_forces && base + count > 0)
       loadcased -> num_forces = base + count;

	/*	
 	 * make a record of everything displayed before we shift anything 
	 * off screen (up shift)
         */

    if (dir == 1) {
       for (i = 0 ; i < count ; i++) {
          loadcased -> node_assignments [base + i] = current [i];
          XtGetValues (loadcased -> force [i], args, 1);
          loadcased -> force_assignments [base + i] = XtNewString (value);
          fprintf (stderr,"force %d = %s\n", base + i, loadcased -> force_assignments [base + i]);
       }
    }

    base = base + dir;
       
	/*
	 * update the nodes that are actually displayed
	 */

    for (i = 0 ; i < 4 ; i++) {
       if (base + i < loadcased -> num_forces) {
          sprintf (buffer,"%d", loadcased -> node_assignments [base + i]);
          SetTextString (loadcased -> node [i], buffer);
          SetTextString (loadcased -> force [i], loadcased -> force_assignments [base + i]);
       }
       else {
	  buffer [0] = 0;
          SetTextString (loadcased -> node [i], buffer);
          SetTextString (loadcased -> force [i], "");
       }

    } 

    loadcased -> node_base = base;

    return;
}

/************************************************************************
 * Function:	ScrollElement	
 *	
 * Description:	 
 ************************************************************************/

static void ScrollElement (Widget w, XtPointer client_data, XtPointer call_data)
{
    Arg		    args [1];
    LoadCaseDialog  loadcased;
    unsigned	    current [4];
    unsigned	    count;
    String	    value;
    char	    buffer [10];
    unsigned	    element;
    unsigned	    i;
    unsigned	    base;
    int     	    dir;

    loadcased = (LoadCaseDialog) client_data;

    base = loadcased -> element_base;

   	/*
	 * figure out which entries have numbers in them
	 */

    XtSetArg (args [0], XtNstring,  &value);
    
    count = 0;
    for (i = 0 ; i < 4 ; i++) {
       XtGetValues (loadcased -> element [i], args, 1);
       element = atoi (value);
       if (element != 0) 
          current [count++] = element;
    }

	/*
	 * if necessary (and allowed) actually do the shift
	 */

    if (w == loadcased -> element_up && count == 4) 
       dir = 1;
    else if (w == loadcased -> element_down && base > 0) 
       dir = -1;
    else
       return;

    fprintf (stderr,"base = %d\n", base);
    fprintf (stderr,"count = %d\n", count);
    fprintf (stderr,"num_loads = %d\n", loadcased -> num_loads);

    if (base + count > loadcased -> num_loads) 
       loadcased -> num_loads = base + count;
    else if (base + count < loadcased -> num_loads && base + count > 0)
       loadcased -> num_loads = base + count;

	/*	
 	 * make a record of everything displayed before we shift anything 
	 * off screen (up shift)
         */

    if (dir == 1) {
       for (i = 0 ; i < count ; i++) {
          loadcased -> element_assignments [base + i] = current [i];
          XtGetValues (loadcased -> load [i], args, 1);
          loadcased -> load_assignments [base + i] = XtNewString (value);
          fprintf (stderr,"load %d = %s\n", base + i, loadcased -> load_assignments [base + i]);
       }
    }

    base = base + dir;
       
	/*
	 * update the elements that are actually displayed
	 */

    for (i = 0 ; i < 4 ; i++) {
       if (base + i < loadcased -> num_loads) {
          sprintf (buffer,"%d", loadcased -> element_assignments [base + i]);
          SetTextString (loadcased -> element [i], buffer);
          SetTextString (loadcased -> load [i], loadcased -> load_assignments [base + i]);
       }
       else {
	  buffer [0] = 0;
          SetTextString (loadcased -> element [i], buffer);
          SetTextString (loadcased -> load [i], "");
       }

    } 

    loadcased -> element_base = base;

    return;
}

/************************************************************************
 * Function:	ChangeForceAssignment	
 *	
 * Description:	 
 ************************************************************************/

static void ChangeForceAssignment (Widget w, XtPointer client_data, XtPointer call_data)
{
    LoadCaseDialog  loadcased;
    Widget	    current_force;
    int		    i;


    if (!(w = XawSimpleMenuGetActiveEntry (w)))
        return;

    loadcased = (LoadCaseDialog) client_data;

    current_force = GetFocus (loadcased -> name);

    for (i = 0 ; i < 4 ; i++) 
       if (current_force == loadcased -> force [i])
          break;

    if (i == 4)
       return;

    if (!strcmp (XtName (w), "- none -"))
        SetTextString (current_force, "");
    else
        SetTextString (current_force, GetLabelString (w));
}


/************************************************************************
 * Function:	ChangeLoadAssignment	
 *	
 * Description:	 
 ************************************************************************/

static void ChangeLoadAssignment (Widget w, XtPointer client_data, XtPointer call_data)
{
    LoadCaseDialog  loadcased;
    Widget          current_load;
    int             i;


    if (!(w = XawSimpleMenuGetActiveEntry (w)))
        return;

    loadcased = (LoadCaseDialog) client_data;

    current_load = GetFocus (loadcased -> name);

    for (i = 0 ; i < 4 ; i++) 
       if (current_load == loadcased -> load [i])
          break;

    if (i == 4)
       return;

    if (!strcmp (XtName (w), "- none -"))
        SetTextString (current_load, "");
    else
        SetTextString (current_load, GetLabelString (w));
}


/************************************************************************
 * Function:	Change							*
 *									*
 * Description:	Changes the displayed values to either the currently	*
 *		selected loadcase if the widget is not null, or the 	*
 *		active loadcase if the widget is null.  The newly 	*
 *		displayed loadcase is made the active loadcase and 	*
 *		any new/copy operation is canceled.			*
 ************************************************************************/

static void Change (Widget w, XtPointer client_data, XtPointer call_data)
{
    char		 buffer [10];
    unsigned		 i;
    LoadCase		 active;
    struct loadcase	 dummy;
    LoadCaseDialog	 loadcased;
    XawListReturnStruct	*info;


    loadcased = (LoadCaseDialog) client_data;


    /* Retrieve the active loadcase from the tree if selected. */

    if (w != NULL) {
	info = (XawListReturnStruct *) call_data;
	if (info -> list_index == XAW_LIST_NONE)
	    return;

	dummy.name = info -> string;
	loadcased -> active = (LoadCase) TreeSearch (loadcased -> tree, &dummy);
    }

    active = loadcased -> active;
    loadcased -> new_copy = False;


    /* Update all of the entries. */

    SetTextString (loadcased -> name, active->name.c_str());
    
    loadcased -> node_base    = 0;
    loadcased -> element_base = 0;
    loadcased -> num_forces = active->forces.size();
    loadcased -> num_loads = active->loads.size();
    
    for (i = 1 ; i <= active->forces.size(); i++) {
        if (i <= 4) {
           sprintf (buffer, "%d", active -> nodes [i] -> number);
           SetTextString (loadcased -> node [i-1], buffer);
           SetTextString (loadcased -> force [i-1], active -> forces [i] -> name.c_str());
        }

        loadcased -> node_assignments [i - 1] = active -> nodes [i] -> number;
        loadcased -> force_assignments [i - 1] = XtNewString (active -> forces [i] -> name.c_str());
    }

    for (i = 1 ; i <= active->loads.size(); i++) {
        if (i <= 4) {
           sprintf (buffer, "%d", active -> elements [i] -> number);
           SetTextString (loadcased -> element [i-1], buffer);
           SetTextString (loadcased -> load [i-1], active -> loads [i] -> name.c_str());
        }

        loadcased -> element_assignments [i - 1] = active -> elements [i] -> number;
        loadcased -> load_assignments [i - 1] = XtNewString (active -> loads [i] -> name.c_str());
    }
}


/************************************************************************
 * Function:	Accept							*
 *									*
 * Description:	Accepts changes made to the current loadcase.		*
 *		If the name is empty or a duplicate name is given then	*
 *		an error is reported.  Otherwise, a new loadcase is	*
 *		created if a new/copy operation is in effect.  The	*
 *		loadcase is then redisplayed to correct any invalid	*
 *		entries.						*
 ************************************************************************/

static void Accept (Widget w, XtPointer client_data, XtPointer call_data)
{
    struct loadcase     old;
    struct loadcase     dummy;
    LoadCase	        found;
    LoadCase	        active;
    Boolean	        duplicate;
    LoadCaseDialog      loadcased;
    struct node		n;
    struct element	e;
    struct force	f;
    struct distributed  l;
    unsigned		i;


    loadcased = (LoadCaseDialog) client_data;


    /* Retrieve the name of the loadcase. */

    dummy.name = GetTextString (loadcased -> name);
    found = (LoadCase) TreeSearch (loadcased -> tree, &dummy);
    duplicate = found && (found != loadcased -> active || loadcased -> new_copy);


    /* Check for a duplicate name. */

    if (!dummy.name [0] || duplicate) {
	XBell (XtDisplay (loadcased -> name), 0);
	SetFocus (loadcased -> name);
	if (!loadcased -> new_copy)
	    SetTextString (loadcased -> name, loadcased->active->name.c_str());
	else
	    SetTextString (loadcased -> name, "");

    } else {


	/* Create a new loadcase or new name as needed. */

	if (loadcased -> new_copy)
	    loadcased -> active = CreateLoadCase(dummy.name.c_str());
	else if (dummy.name != loadcased->active->name) {
            old.name = loadcased -> active -> name;
            TreeDelete (loadcased -> tree, &old);
            loadcased -> active -> name = dummy.name;
            TreeInsert (loadcased -> tree, loadcased -> active);
	}

	active = loadcased -> active;

	/*
	 * fill out the actual components of the active load case here
	 */


        if (loadcased -> num_forces) {
           active->nodes.clear();
           active->nodes.resize(loadcased->num_forces);
           
           active->forces.clear();
           active->forces.resize(loadcased->num_forces);
           
           for (i = 1 ; i <= active->forces.size(); i++) {
              n.number = loadcased -> node_assignments [i-1];
              Problem::NodeSet::iterator it = problem.node_set.find(&n);
              active -> nodes [i] = it != problem.node_set.end() ? *it : NULL;
              if (active -> nodes [i] == NULL) {
                 error ("node %d is not defined", n.number);
                 return;
              }

              f.name = loadcased -> force_assignments [i-1];
              active -> forces [i] = (Force) TreeSearch (problem.force_tree, &f);
              if (active -> forces [i] == NULL) {
                  error ("force %s is not defined", f.name.c_str());
                 return;
              }
           }
        }

        if (loadcased -> num_loads) {
            active->loads.clear();
            active->loads.resize(loadcased->num_loads);
            
            active->elements.clear();
            active->elements.resize(loadcased->num_loads);

            for (i = 1 ; i <= active->loads.size(); i++) {
              e.number = loadcased -> element_assignments [i-1];
              Problem::ElementSet::iterator it = problem.element_set.find(&e);
              active -> elements [i] = it != problem.element_set.end() ? *it : NULL;
              if (active -> elements [i] == NULL) {
                 error ("element %d is not defined", e.number);
                 return;
              }

              l.name = loadcased -> load_assignments [i-1];
              Problem::DistributedSet::iterator itd = problem.distributed_set.find(&l);
              active -> loads [i] = itd != problem.distributed_set.end() ? *itd : NULL;
              if (active -> loads [i] == NULL) {
                  error ("distributed load %s is not defined", l.name.c_str());
                 return;
              }
           }
        }

	if (loadcased -> new_copy)
	    TreeInsert (loadcased -> tree, loadcased -> active);

	LoadCaseDialogUpdate (loadcased, loadcased -> tree, NULL, NULL);
    }
}


/************************************************************************
 * Function:	Dismiss							*
 *									*
 * Description:	Pops down the dialog box.				*
 ************************************************************************/

static void Dismiss (Widget w, XtPointer client_data, XtPointer call_data)
{
    LoadCaseDialog loadcased;


    loadcased = (LoadCaseDialog) client_data;
    XtPopdown (loadcased -> shell);
}


/************************************************************************
 * Function:	Delete							*
 *									*
 * Description:	Deletes the active loadcase if a new/copy operation is 	*
 *		not in effect.  The dialog is then updated.		*
 ************************************************************************/

static void Delete (Widget w, XtPointer client_data, XtPointer call_data)
{
    LoadCaseDialog	    loadcased;


    loadcased = (LoadCaseDialog) client_data;

    if (!loadcased -> new_copy) {
	TreeDelete (loadcased -> tree, loadcased -> active);
	DestroyLoadCase (loadcased -> active);
	loadcased -> active = NULL;
    }

    LoadCaseDialogUpdate (loadcased, loadcased -> tree, NULL, NULL);
}


/************************************************************************
 * Function:	Copy							*
 *									*
 * Description:	Clears the name entry only and sets the flag indicating	*
 *		that a new/copy operation is in effect.			*
 ************************************************************************/

static void Copy (Widget w, XtPointer client_data, XtPointer call_data)
{
    LoadCaseDialog loadcased;


    loadcased = (LoadCaseDialog) client_data;

    loadcased -> new_copy = True;
    SetFocus (loadcased -> name);
    XawListUnhighlight (loadcased -> list);
    SetTextString (loadcased -> name, "");
}


/************************************************************************
 * Function:	New							*
 *									*
 * Description:	Clears all entries in the loadcase dialog and sets the	*
 *		flag indicating that a new/copy operation is in effect.	*
 ************************************************************************/

static void New (Widget w, XtPointer client_data, XtPointer call_data)
{
    LoadCaseDialog loadcased;
    unsigned       i;


    loadcased = (LoadCaseDialog) client_data;

    Copy (NULL, client_data, NULL);
    for (i = 0 ; i < 4 ; i++) {
       SetTextString (loadcased -> node [i] , "");
       SetTextString (loadcased -> element [i], "");
    }

    loadcased -> num_forces = 0;
    loadcased -> num_loads = 0;
    loadcased -> node_base = 0;
    loadcased -> element_base = 0;
}


/************************************************************************
 * Function:	LoadCaseDialogCreate					*
 *									*
 * Description:	Creates a new loadcase dialog.  You would never want to	*
 *		have more than one of these but the interface is kept	*
 *		consistent with those of the other dialogs.		*
 ************************************************************************/

LoadCaseDialog LoadCaseDialogCreate (Widget parent, String name, String title)
{
    Window		window;
    Cardinal		i;
    char		buffer [128];
    Arg			args [2];
    Widget		group [28];
    LoadCaseDialog	loadcased;
    Dimension		width;
    Position		x;
    static XtAppContext	app_context = NULL;
    static XtActionsRec	actions [ ] = {{"LoadCaseDialogAction", Action}};


    /* Perform one time initialization. */

    if (app_context == NULL) {
	app_context = XtWidgetToApplicationContext (parent);
	XtAppAddActions (app_context, actions, XtNumber (actions));
	AddAutoRepeatAction (app_context);

        layout_args [0].value = StringToLayout (parent, layout_string);

	text_translations     = XtParseTranslationTable (text_table);
	command_translations  = XtParseTranslationTable (command_table);
	viewport_translations = XtParseTranslationTable (viewport_table);
	help_translations     = XtParseTranslationTable (help_table);
	repeater_translations = XtParseTranslationTable (repeater_table);
	button_translations   = XtParseTranslationTable (button_table);

        window = RootWindowOfScreen (XtScreen (parent));

        up_bitmap = XCreateBitmapFromData (XtDisplay (parent), window,
                    up_bits, up_width, up_height);

        down_bitmap = XCreateBitmapFromData (XtDisplay (parent), window,
                      down_bits, down_width, down_height);
    }


    /* Create the loadcase dialog and its widgets. */

    XtSetArg (shell_args [0], XtNtitle, title);
    XtSetArg (shell_args [1], XtNiconName, title);

    loadcased = XtNew (struct loadcase_dialog);

    loadcased -> loadcases = NULL;

    loadcased -> active   = NULL;

    loadcased -> shell    = XtCreatePopupShell (name,
			 topLevelShellWidgetClass, parent,
			 shell_args, XtNumber (shell_args));

    loadcased -> layout   = XtCreateManagedWidget ("layout",
			 layoutWidgetClass, loadcased -> shell,
			 layout_args, XtNumber (layout_args));

    loadcased -> viewport = XtCreateManagedWidget ("viewport",
			 viewportWidgetClass, loadcased -> layout,
			 viewport_args, XtNumber (viewport_args));

    loadcased -> list     = XtCreateManagedWidget ("list",
			 listWidgetClass, loadcased -> viewport,
			 list_args, XtNumber (list_args));

    loadcased -> name     = XtCreateManagedWidget ("name",
			 asciiTextWidgetClass, loadcased -> layout,
			 name_args, XtNumber (name_args));

    for (i = 0 ; i < 4 ; i++) {
       sprintf (buffer,"node%d", i+1);
       loadcased -> node [i] = XtCreateManagedWidget (XtNewString (buffer),
			 asciiTextWidgetClass, loadcased -> layout,
			 number_args, XtNumber (number_args));

       sprintf (buffer,"element%d", i+1);
       loadcased -> element [i] = XtCreateManagedWidget (XtNewString (buffer),
			 asciiTextWidgetClass, loadcased -> layout,
			 number_args, XtNumber (number_args));

       sprintf (buffer,"force%d", i+1);
       loadcased -> force [i] = XtCreateManagedWidget (XtNewString (buffer),
			 asciiTextWidgetClass, loadcased -> layout,
			 name_args, XtNumber (name_args));

       sprintf (buffer,"load%d", i+1);
       loadcased -> load [i] = XtCreateManagedWidget (XtNewString (buffer),
			 asciiTextWidgetClass, loadcased -> layout,
			 name_args, XtNumber (name_args));
    }

    loadcased -> force_menu = XtCreateManagedWidget ("force_menu",
			 simpleMenuWidgetClass, loadcased -> layout,
			 NULL, 0);

    XtSetArg (button_args [0], XtNlabel,    "forces");
    XtSetArg (button_args [1], XtNmenuName, "force_menu");

    loadcased -> force_button = XtCreateManagedWidget ("force_button",
			 menuButtonWidgetClass, loadcased -> layout,
			 button_args, XtNumber (button_args));

    loadcased -> load_menu = XtCreateManagedWidget ("load_menu",
			 simpleMenuWidgetClass, loadcased -> layout,
			 NULL, 0);

    XtSetArg (button_args [0], XtNlabel,    "loads");
    XtSetArg (button_args [1], XtNmenuName, "load_menu");

    loadcased -> load_button = XtCreateManagedWidget ("load_button",
			 menuButtonWidgetClass, loadcased -> layout,
			 button_args, XtNumber (button_args));

    XtCreateManagedWidget ("- none -", smeBSBObjectClass,
                           loadcased -> force_menu, NULL, 0);

    XtCreateManagedWidget ("- none -", smeBSBObjectClass,
                           loadcased -> load_menu, NULL, 0);

    AddPostMenuActions (loadcased -> force_menu);
    AddPostMenuActions (loadcased -> load_menu);

    repeater_args [0].value = (XtArgVal) down_bitmap;

    loadcased -> node_down = XtCreateManagedWidget ("node_down",
                          repeaterWidgetClass, loadcased -> layout,
                          repeater_args, XtNumber (repeater_args));

    loadcased -> element_down = XtCreateManagedWidget ("element_down",
                          repeaterWidgetClass, loadcased -> layout,
                          repeater_args, XtNumber (repeater_args));

    repeater_args [0].value = (XtArgVal) up_bitmap;

    loadcased -> node_up = XtCreateManagedWidget ("node_up",
                          repeaterWidgetClass, loadcased -> layout,
                          repeater_args, XtNumber (repeater_args));

    loadcased -> element_up = XtCreateManagedWidget ("element_up",
                          repeaterWidgetClass, loadcased -> layout,
                          repeater_args, XtNumber (repeater_args));

    loadcased -> accept   = XtCreateManagedWidget ("accept",
			 commandWidgetClass, loadcased -> layout,
			 NULL, 0);

    loadcased -> dismiss  = XtCreateManagedWidget ("dismiss",
			 commandWidgetClass, loadcased -> layout,
			 NULL, 0);

    loadcased -> nuke   = XtCreateManagedWidget ("delete",
			 commandWidgetClass, loadcased -> layout,
			 NULL, 0);

    loadcased -> nu      = XtCreateManagedWidget ("new",
			 commandWidgetClass, loadcased -> layout,
			 NULL, 0);

    loadcased -> copy     = XtCreateManagedWidget ("copy",
			 commandWidgetClass, loadcased -> layout,
			 NULL, 0);

    loadcased -> help     = CreateHelpButton (loadcased -> layout, "help");

    for (i = 0; i < XtNumber (labels); i ++) {
	label_args [0].value = (XtArgVal) labels [i];
	XtCreateManagedWidget (names [i], labelWidgetClass,
		loadcased -> layout, label_args, XtNumber (label_args));
    }


    XtCreateManagedWidget ("separator1", coreWidgetClass,
			loadcased -> layout, core_args, XtNumber (core_args));
    XtCreateManagedWidget ("separator2", coreWidgetClass,
			loadcased -> layout, core_args, XtNumber (core_args));
    XtCreateManagedWidget ("separator3", coreWidgetClass,
			loadcased -> layout, core_args, XtNumber (core_args));



    /* Create a tab group for the loadcase dialog. */

    i = 0;
    group [i++]  = loadcased -> name;
    group [i++]  = loadcased -> viewport;
    group [i++]  = loadcased -> node[0];
    group [i++]  = loadcased -> force[0];
    group [i++]  = loadcased -> node[1];
    group [i++]  = loadcased -> force[1];
    group [i++]  = loadcased -> node[2];
    group [i++]  = loadcased -> force[2];
    group [i++]  = loadcased -> node[3];
    group [i++]  = loadcased -> force[3];
    group [i++]  = loadcased -> node_down;
    group [i++]  = loadcased -> node_up;
    group [i++]  = loadcased -> element[0];
    group [i++]  = loadcased -> load[0];
    group [i++]  = loadcased -> element[1];
    group [i++]  = loadcased -> load[1];
    group [i++]  = loadcased -> element[2];
    group [i++]  = loadcased -> load[2];
    group [i++]  = loadcased -> element[3];
    group [i++]  = loadcased -> load[3];
    group [i++]  = loadcased -> element_down;
    group [i++]  = loadcased -> element_up;
    group [i++]  = loadcased -> help;
    group [i++]  = loadcased -> accept;
    group [i++] = loadcased -> dismiss;
    group [i++] = loadcased -> nuke;
    group [i++] = loadcased -> nu;
    group [i++] = loadcased -> copy;

    XtGetValues (loadcased -> layout, color_args, XtNumber (color_args));
    CreateTabGroup (loadcased -> shell, group, XtNumber (group), highlight, True);
    XtRealizeWidget (loadcased -> shell);
    SetFocus (loadcased -> name);

    XtSetArg (args [0], XtNwidth, &width);
    XtGetValues (loadcased -> layout, args, 1);
    XtSetArg (args [0], XtNx, &x);
    XtGetValues (loadcased -> help, args, 1);
    UpdateHelpMessage (loadcased -> help, help_message, width - 2 * x);


    /* Add the translations to each widget. */

    AddDeleteWindowProtocol   (loadcased -> shell, "LoadCaseDialogAction()");
    ListAddCursorTranslations (loadcased -> viewport);

    XtOverrideTranslations (loadcased -> name,	   text_translations);

    XtOverrideTranslations (loadcased -> node[0],    text_translations);
    XtOverrideTranslations (loadcased -> node[1],    text_translations);
    XtOverrideTranslations (loadcased -> node[2],    text_translations);
    XtOverrideTranslations (loadcased -> node[3],    text_translations);
    XtOverrideTranslations (loadcased -> force[0],    text_translations);
    XtOverrideTranslations (loadcased -> force[1],    text_translations);
    XtOverrideTranslations (loadcased -> force[2],    text_translations);
    XtOverrideTranslations (loadcased -> force[3],    text_translations);
    XtOverrideTranslations (loadcased -> element[0], text_translations);
    XtOverrideTranslations (loadcased -> element[1], text_translations);
    XtOverrideTranslations (loadcased -> element[2], text_translations);
    XtOverrideTranslations (loadcased -> element[3], text_translations);
    XtOverrideTranslations (loadcased -> load[0], text_translations);
    XtOverrideTranslations (loadcased -> load[1], text_translations);
    XtOverrideTranslations (loadcased -> load[2], text_translations);
    XtOverrideTranslations (loadcased -> load[3], text_translations);

    XtOverrideTranslations (loadcased -> force_button, button_translations);
    XtOverrideTranslations (loadcased -> load_button, button_translations);

    XtOverrideTranslations (loadcased -> element_up,   repeater_translations);
    XtOverrideTranslations (loadcased -> element_down, repeater_translations);
    XtOverrideTranslations (loadcased -> node_up,      repeater_translations);
    XtOverrideTranslations (loadcased -> node_down,    repeater_translations);

    XtOverrideTranslations (loadcased -> accept,    command_translations);
    XtOverrideTranslations (loadcased -> dismiss,   command_translations);
    XtOverrideTranslations (loadcased -> nuke,    command_translations);
    XtOverrideTranslations (loadcased -> nu,	    command_translations);
    XtOverrideTranslations (loadcased -> copy,	    command_translations);
    XtOverrideTranslations (loadcased -> viewport,  viewport_translations);
    XtOverrideTranslations (loadcased -> help,	    help_translations);


    /* Add the necessary callbacks. */

    XtAddCallback (loadcased -> list,    XtNcallback, Change,  (XtPointer) loadcased);
    XtAddCallback (loadcased -> accept,  XtNcallback, Accept,  (XtPointer) loadcased);
    XtAddCallback (loadcased -> dismiss, XtNcallback, Dismiss, (XtPointer) loadcased);
    XtAddCallback (loadcased -> nuke,  XtNcallback, Delete,  (XtPointer) loadcased);
    XtAddCallback (loadcased -> nu,     XtNcallback, New,     (XtPointer) loadcased);
    XtAddCallback (loadcased -> copy,    XtNcallback, Copy,    (XtPointer) loadcased);
    XtAddCallback (loadcased -> node_up,      XtNcallback, ScrollNode,    (XtPointer) loadcased);
    XtAddCallback (loadcased -> node_down,    XtNcallback, ScrollNode,    (XtPointer) loadcased);
    XtAddCallback (loadcased -> element_up,   XtNcallback, ScrollElement, (XtPointer) loadcased);
    XtAddCallback (loadcased -> element_down, XtNcallback, ScrollElement, (XtPointer) loadcased);
    XtAddCallback (loadcased -> force_menu, XtNpopdownCallback, ChangeForceAssignment, (XtPointer) loadcased); 
    XtAddCallback (loadcased -> load_menu,  XtNpopdownCallback, ChangeLoadAssignment, (XtPointer) loadcased); 

    loadcased -> node_base = 0;
    loadcased -> element_base = 0;
    loadcased -> num_forces = 0;
    loadcased -> num_loads = 0;

    return loadcased;
}


/************************************************************************
 * Function:	LoadCaseDialogPopup					*
 *									*
 * Description:	Pops up the specified loadcase dialog.			*
 ************************************************************************/

void LoadCaseDialogPopup (LoadCaseDialog loadcased)
{
    XtPopup (loadcased -> shell, XtGrabNone);
}


/************************************************************************
 * Function:	LoadCaseDialogActive					*
 *									*
 * Description:	Returns the currently active loadcase.			*
 ************************************************************************/

LoadCase LoadCaseDialogActive (LoadCaseDialog loadcased)
{
    return loadcased -> active;
}


/************************************************************************
 * Function:	LoadCaseDialogDisplay					*
 *									*
 * Description:	Displays a specified loadcase.				*
 ************************************************************************/

void LoadCaseDialogDisplay (LoadCaseDialog loadcased, LoadCase loadcase)
{
    loadcased -> active = loadcase;
    LoadCaseDialogUpdate (loadcased, loadcased -> tree, NULL, NULL);
}

/* Menu creation variables */

static Cardinal         child_number;
static WidgetList       children;
static Dimension        max_width;
static XtWidgetGeometry preferred;

/************************************************************************
 * Function:    SetForceEntry                                           *
 *                                                                      *
 * Description: Sets the label of the next menu entry to the name of    *
 *              the specified force.                                    *
 ************************************************************************/

static int SetForceEntry (Item item)
{
    SetLabelString (children [child_number], ((Force) item) -> name.c_str());

    XtQueryGeometry (children [child_number ++], NULL, &preferred);
    if (preferred.width > max_width)
        max_width = preferred.width;

    return 0;
}

/************************************************************************
 * Function:    SetLoadEntry                                            *
 *                                                                      *
 * Description: Sets the label of the next menu entry to the name of    *
 *              the specified load.                                     *
 ************************************************************************/

static int SetLoadEntry (Distributed item)
{
    SetLabelString (children [child_number], item -> name.c_str());

    XtQueryGeometry (children [child_number ++], NULL, &preferred);
    if (preferred.width > max_width)
        max_width = preferred.width;

    return 0;
}

/************************************************************************
 * Function:	LoadCaseDialogUpdate					*
 *									*
 * Description:	Updates the given loadcase dialog with the specified	*
 *		tree.  If no active loadcase exists, the first loadcase *
 *		is made active.  If no active loadcase still exists 	*
 *		then a new operation is performed.  Otherwise a change	*
 *		operation is performed to display the active values.	*
 ************************************************************************/

void LoadCaseDialogUpdate (LoadCaseDialog loadcased, Tree tree, Tree force_tree, Problem::DistributedSet *load_tree)
{
    char	buffer [32];
    Arg		args [2];
    Cardinal 	nbytes;
    Cardinal	num_forces;
    Cardinal	num_loads;
    Cardinal	num_children;
    Cardinal	i;
    Cardinal	overflow;


    /* Determine a new active loadcase if necessary. */

    if (tree == NULL)
	tree = loadcased -> tree;

    if (loadcased -> active == NULL || tree != loadcased -> tree)
	loadcased -> active = (LoadCase) TreeMinimum (tree);


    /* Construct the array of loadcase names. */

    num_loadcases = 0;
    list_index = -1;
    dialog = loadcased;
    loadcased -> tree = tree;
    loadcased -> new_copy = False;

    if (loadcased->loadcases != NULL) {
        for (size_t i = 0; loadcased->loadcases[i]; i++)
            XtFree(loadcased->loadcases[i]);
    }
    
    nbytes = (TreeSize (loadcased -> tree) + 1) * sizeof (String);
    loadcased -> loadcases = (String *) XtRealloc ((char *) loadcased -> loadcases, nbytes);

    TreeSetIterator (loadcased -> tree, AppendLoadCaseName);
    TreeIterate (loadcased -> tree);
    loadcased -> loadcases [num_loadcases] = NULL;


    /* Update the list widget. */

    XawListChange (loadcased -> list, loadcased -> loadcases, 0, 0, True);

    if (list_index >= 0)
	XawListHighlight (loadcased -> list, list_index);


    /* Update the force menu */

   
    if (force_tree != NULL) {
       XtSetArg (args [0], XtNchildren,    &children);
       XtSetArg (args [1], XtNnumChildren, &num_children);
       XtGetValues (loadcased -> force_menu, args, 2);

       num_forces = TreeSize (force_tree) + 1;

       for (i = num_children; i < num_forces; i ++) {
           sprintf (buffer, "force_entry%d", i);
           XtCreateManagedWidget (XtNewString (buffer), smeBSBObjectClass,
                                  loadcased -> force_menu, NULL, 0);
       }

       if (num_children > num_forces) {
           overflow = num_children - num_forces;
           XtUnmanageChildren (children + num_forces, overflow);
       }

       XtSetArg (args [0], XtNchildren,    &children);
       XtSetArg (args [1], XtNnumChildren, &num_children);
       XtGetValues (loadcased -> force_menu, args, 2);

       XtQueryGeometry (children [0], NULL, &preferred);
       max_width = preferred.width;

       child_number = 1;
       TreeSetIterator (force_tree, SetForceEntry);
       TreeIterate (force_tree);

       XtSetArg (args [0], XtNwidth, max_width);
       XtSetValues (loadcased -> force_menu, args, 1);
    }


    /* Update the load menu */

   
    if (load_tree != NULL) {
       XtSetArg (args [0], XtNchildren,    &children);
       XtSetArg (args [1], XtNnumChildren, &num_children);
       XtGetValues (loadcased -> load_menu, args, 2);

       num_loads = load_tree->size() + 1;

       for (i = num_children; i < num_loads; i ++) {
           sprintf (buffer, "load_entry%d", i);
           XtCreateManagedWidget (XtNewString (buffer), smeBSBObjectClass,
                                  loadcased -> load_menu, NULL, 0);
       }

       if (num_children > num_loads) {
           overflow = num_children - num_loads;
           XtUnmanageChildren (children + num_loads, overflow);
       }

       XtSetArg (args [0], XtNchildren,    &children);
       XtSetArg (args [1], XtNnumChildren, &num_children);
       XtGetValues (loadcased -> load_menu, args, 2);

       XtQueryGeometry (children [0], NULL, &preferred);
       max_width = preferred.width;
   
       child_number = 1;
       std::for_each(load_tree->begin(), load_tree->end(), SetLoadEntry);
   
       XtSetArg (args [0], XtNwidth, max_width);
       XtSetValues (loadcased -> load_menu, args, 1);
    }

    /* Update the text entries. */

    if (loadcased -> active == NULL)
	New (NULL, (XtPointer) loadcased, NULL);
    else
	Change (NULL, (XtPointer) loadcased, NULL);
}
