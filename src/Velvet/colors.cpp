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
 * File:	colors.c						*
 *									*
 * Description:	This file contains the public and private function and	*
 *		type definitions for the colors dialog box.		*
 ************************************************************************/

# include <algorithm>
# include <stdio.h>
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Shell.h>
# include <X11/Xaw/Command.h>
# include <X11/Xaw/Label.h>
# include <X11/Xaw/List.h>
# include <X11/Xaw/Viewport.h>
# include "Layout.h"
# include "TabGroup.h"
# include "problem.h"
# include "Colors.h"
# include "util.h"
# include "objects.h"

# ifndef X_NOT_STDC_ENV
# include <stdlib.h>
# endif

# ifndef XtNshadowWidth
# define XtNshadowWidth "shadowWidth"
# endif

# if NeedWidePrototypes
# define BOOLEAN int
# else
# define BOOLEAN Boolean
# endif

extern  void RecolorCanvas (void);

struct colors_dialog {
    Widget         shell;	/* topLevelShell  <specified>	 */
    Widget         layout;	/*	Layout  layout		 */
    Widget	   mview;	/*	     Viewport mview	 */
    Widget	   mlist;	/*                List mlist	 */
    Widget	   cview;	/*	     Viewport cview	 */
    Widget	   clist;	/*                List clist	 */
    Widget	   lview;	/*	     Viewport lview	 */
    Widget	   llist;	/*                List llist	 */
    Widget	   fview;	/*	     Viewport fview	 */
    Widget	   flist;	/*                List flist	 */
    Widget	   colorview;	/*	     Viewport colorview  */
    Widget	   colorlist;	/*	 	  List colorlist */
    Widget         help;	/*	     MenuButton  help	 */
    Widget         dismiss;	/*	     Command  dismiss	 */
    Widget         recolor;	/*	     Command  recolor	 */
    String        *colors;	/* array of color names		 */
    String        *materials;
    String        *forces;
    String        *constraints;
    String        *loads;
    unsigned	   numcolors;	/* number of color names	 */
};

static String labels [ ] = {
    "Forces", "Materials", "Loads", "Constraints", "Colors"
};

static String names [ ] = {
   "forceLabel", "materialLabel", "loadLabel", "constraintLabel", "colorLabel"
};

static Material		current_material;
static Constraint	current_constraint;
static Distributed	current_load;
static Force		current_force;
static Widget		active_list = NULL;

/* Resources */

static Pixel highlight;

static String layout_string =
"vertical { \
     horizontal { \
 	8 \
 	vertical { \
	    4 \
            horizontal { \
               4 <+inf> \
               ((width constraintLabel - width materialLabel) / 2) <+inf> \
	       materialLabel \
               ((width constraintLabel - width materialLabel) / 2) <+inf> \
               4 <+inf> \
            } \
 	    4 \
 	    mview <+inf -100% * +inf -100%> \
 	    12 \
            horizontal { \
               4 <+inf> \
               ((width constraintLabel - width loadLabel) / 2) <+inf> \
               loadLabel \
               ((width constraintLabel - width loadLabel) / 2) <+inf> \
               4 <+inf> \
            } \
            4 \
            lview <+inf -100% * +inf -100%> \
            4 \
 	} \
 	8 \
 	vertical { \
 	    4 \
            horizontal { \
               4 <+inf> \
	       constraintLabel \
               4 <+inf> \
            } \
 	    4 \
 	    cview <+inf -100% * +inf -100%> \
 	    12 \
            horizontal { \
               4 <+inf> \
               ((width constraintLabel - width forceLabel) / 2) <+inf> \
               forceLabel \
               ((width constraintLabel - width forceLabel) / 2) <+inf> \
               4 <+inf> \
            } \
            4 \
            fview <+inf -100% * +inf -100%> \
	    4 \
 	} \
 	4 \
        separator1 <* +inf -100%> \
        4 \
        vertical { \
            4 \
            horizontal { \
               4 \
               4 <+inf> \
               colorLabel \
               4 <+inf> \
               4 \
            } \
            4 \
            colorview <+inf -100% * +inf -100%> \
            4 \
        } \
	8 \
     } \
     separator2 <+inf -100% *> \
     4 \
     horizontal { \
 	4 \
 	help \
 	4 <+inf -100%> \
        recolor \
 	4 <+inf -100%> \
 	dismiss \
 	4 \
     } \
     4 \
}";

static String dummy_list [ ] = {
    NULL
};

static String default_colors [ ] = {
    "black", "white", "red", "blue", "green", "yellow", "orange",
    "purple", "cyan", "beige", "brown", "grey", "pink", "magenta", NULL
};

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

static Arg colorlist_args [ ] = {
    {XtNdefaultColumns, (XtArgVal) 1},
    {XtNforceColumns,   (XtArgVal) 1},
    {XtNresize,		(XtArgVal) True},
    {XtNlist,		(XtArgVal) default_colors},
};

static Arg label_args [ ] = {
    {XtNlabel,       (XtArgVal) ""},
    {XtNborderWidth, (XtArgVal) 0},
};

static Arg core_args [ ] = {
    {XtNwidth,  (XtArgVal) 3},
    {XtNheight, (XtArgVal) 3},
};


/* Translation tables */

static String command_table =
"<Key>Escape:  ColorsDialogAction(dismiss)\n\
 Ctrl<Key>r:   ColorsDialogAction(recolor)\n\
 Ctrl<Key>h:   ColorsDialogAction(help)\n\
 <Key>space:   AutoRepeat(off) set()\n\
 <KeyUp>space: AutoRepeat(saved) notify() unset()";

static XtTranslations command_translations;

static String viewport_table =
"<Key>Escape: ColorsDialogAction(dismiss)\n\
 Ctrl<Key>r:  ColorsDialogAction(recolor)\n\
 Ctrl<Key>h:  ColorsDialogAction(help)\n\
 <Btn1Down>:  SetFocus()";

static XtTranslations viewport_translations;


static String help_table =
"<Key>Escape: ColorsDialogAction(dismiss)\n\
 Ctrl<Key>r:  ColorsDialogAction(recolor)\n\
 Ctrl<Key>h:  ColorsDialogAction(help)\n\
 <Key>space:  PostMenu()";

static XtTranslations help_translations;


/* Help message. */

static String help_message ="\
The control lists in this dialog allow you to assign a unique color to each \
object in your problem.  To assign a color, click on an object in one of \
the lists and then click on an item in the color list.  Changing the selected \
item in the color list automatically changes the color assigned to the \
currently selected object.\
";


/************************************************************************
 * Function:	ExternalCallback					*
 *									*
 * Description:	Call an external routine based on a button press	*
 ************************************************************************/

static void ExternalCallback (Widget w, XtPointer client_data, XtPointer call_data)
{
   ColorsDialog	colorsd;

   colorsd = (ColorsDialog) client_data;

   if (w == colorsd -> recolor)
      RecolorCanvas ( );

   return;
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
 * Function:	FindColorIndex						*
 *									*
 * Description:	returns the index into the color array for the given	*
 *		color name						*
 ************************************************************************/

static int FindColorIndex (ColorsDialog colorsd, const char *color)
{
   unsigned	i;

   for (i = 0 ; i < colorsd -> numcolors - 1 ; i++)
      if (strcmp (color, colorsd -> colors [i]) == 0)
         return i;
   
   return -1;
}

/************************************************************************
 * Function:	ObjectChange						*	
 *									*
 * Description:	changes the currently active list and the current	*
 *		object within that list					*
 *									*	
 ************************************************************************/

static void ObjectChange (Widget w, XtPointer client_data, XtPointer call_data)
{
    ColorsDialog	colorsd;
    XawListReturnStruct	*info;
    std::string color = "";
    struct material	m;
    struct constraint	c;
    struct force	f;
    struct distributed	l;
    int			item;
  
    colorsd = (ColorsDialog) client_data;

    if (active_list != w && active_list != NULL)
       XawListUnhighlight (active_list);

    active_list = w;

    info = (XawListReturnStruct *) call_data;
    if (info -> list_index == XAW_LIST_NONE)
       return;

    if (active_list == colorsd -> mlist) {
       m.name = info -> string;
       current_material = *problem.material_set.find(&m);
       color = current_material -> color;
    }
    else if (active_list == colorsd -> clist) {
       c.name = info -> string;
       current_constraint = (Constraint) TreeSearch (problem.constraint_tree, &c);
       color = current_constraint -> color;
    }
    else if (active_list == colorsd -> flist) {
       f.name = info -> string;
       current_force = (Force) TreeSearch (problem.force_tree, &f);
       color = current_force -> color;
    }
    else if (active_list == colorsd -> llist) {
       l.name = info -> string;
       current_load = (Distributed) TreeSearch (problem.distributed_tree, &l);
       color = current_load -> color;
    }

    if (!color.empty()) {
        item = FindColorIndex (colorsd, color.c_str());
       if (item != -1)
          XawListHighlight (colorsd -> colorlist, item);
       else
          XawListUnhighlight (colorsd -> colorlist);
    } 
    else
       XawListUnhighlight (colorsd -> colorlist);

    return;
}

/************************************************************************
 * Function:	ColorChange						*
 *									*
 * Description:	changes the color of the currently active object	*
 ************************************************************************/

static void ColorChange (Widget w, XtPointer client_data, XtPointer call_data)
{
    ColorsDialog	colorsd;
    XawListReturnStruct	*info;
  
    colorsd = (ColorsDialog) client_data;
    
    info = (XawListReturnStruct *) call_data;
    if (info -> list_index == XAW_LIST_NONE)
       return;

    if (active_list == colorsd -> mlist) {
       current_material -> color = info -> string;
    }
    else if (active_list == colorsd -> llist) {
        current_load -> color = info -> string; 
    }
    else if (active_list == colorsd -> clist) {
        current_constraint -> color = info -> string;
    }
    else if (active_list == colorsd -> flist) {
        current_force -> color = info -> string;
    }
    else 
       XawListUnhighlight (colorsd -> colorlist);

    return;
}

/************************************************************************
 * Function:	Dismiss							*
 *									*
 * Description:	Pops down the dialog box.				*
 ************************************************************************/

static void Dismiss (Widget w, XtPointer client_data, XtPointer call_data)
{
    ColorsDialog colorsd;

    colorsd = (ColorsDialog) client_data;
    XtPopdown (colorsd -> shell);
}

/************************************************************************
 * Function:	InsertColor 						*
 *									*
 * Description:	Inserts a color name into the colors array and list	*
 *		if it is not already there				*
 ************************************************************************/

static void
InsertColor(ColorsDialog colorsd, String color)
{
   int		index;

   if (color == NULL)
      return;

   index = FindColorIndex (colorsd, color);
   if (index != -1)
      return;

   colorsd -> numcolors ++;

   colorsd -> colors = (String *) XtRealloc ((char *) colorsd -> colors,
                                             (colorsd -> numcolors)*sizeof(char *));
   
   colorsd -> colors [colorsd -> numcolors - 2] = XtNewString (color);
   colorsd -> colors [colorsd -> numcolors - 1] = NULL;

   XawListChange (colorsd -> colorlist, colorsd -> colors, 0, 0, True);
     
   return; 
}

static int	 object_count;
static String	*object_names = NULL;
static String	*object_colors = NULL;

static int AddMaterial (Material item)
{
    object_colors [object_count] = XtNewString(((Material) item) -> color.c_str());
    object_names [object_count ++] = XtNewString(((Material) item) -> name.c_str());
   return 0;
}

static int AddConstraint (Item item)
{
    object_colors [object_count] = XtNewString(((Constraint) item) -> color.c_str());
    object_names [object_count ++] = XtNewString(((Constraint) item) -> name.c_str());
   return 0;
}

static int AddLoad (Item item)
{
    object_colors [object_count] = XtNewString(((Distributed) item) -> color.c_str());
    object_names [object_count ++] = XtNewString(((Distributed) item) -> name.c_str());
   return 0;
}

static int AddForce (Item item)
{
    object_colors [object_count] = XtNewString(((Force) item) -> color.c_str());
    object_names [object_count ++] = XtNewString(((Force) item) -> name.c_str());
   return 0;
}

/************************************************************************
 * Function:	UpdateList						*
 *									*
 * Description:	Does the actual dirty work for a given list and tree	*
 ************************************************************************/

static void UpdateList (Widget w, Tree tree, Boolean deleted)
{
   unsigned	size;
   unsigned	number_of_items;

   object_count = 0;
  
   number_of_items = TreeSize (tree);
   if (deleted)
      number_of_items --;
 
   size = (number_of_items + 1)*sizeof (String);
   object_names = (String *) XtRealloc ((char *) object_names, size);
   object_colors = (String *) XtRealloc ((char *) object_colors, size);

   TreeIterate (tree);

   object_names [number_of_items] = NULL;

   XawListChange (w, object_names, 0, 0, False);

   return;
}

template<typename Set, typename Unary>
void UpdateList (Widget w, Set tree, Boolean deleted, Unary fun)
{
   unsigned	size;
   unsigned	number_of_items;

   object_count = 0;
  
   number_of_items = tree->size();
   if (deleted)
      number_of_items --;
 
   size = (number_of_items + 1)*sizeof (String);
   object_names = (String *) XtRealloc ((char *) object_names, size);
   object_colors = (String *) XtRealloc ((char *) object_colors, size);

   std::for_each(tree->begin(), tree->end(), fun);

   object_names [number_of_items] = NULL;

   XawListChange (w, object_names, 0, 0, False);

   return;
}

/************************************************************************
 * Function:	ColorsDialogUpdateObjectList				*
 *									*
 * Description:	Sets the list of names for one of the given object	*
 *		lists; the list is determined from the tree that	*
 *	 	is passed in.						*
 ************************************************************************/

void ColorsDialogUpdateObjectList (ColorsDialog colorsd, Tree tree, Boolean deleted)
{
   unsigned	i;

   if (tree == problem.constraint_tree) {
      TreeSetIterator (tree, AddConstraint);
      object_names = colorsd -> constraints;
      UpdateList (colorsd -> clist, tree, deleted);
      colorsd -> constraints = object_names;
   }
 
   else if (tree == problem.distributed_tree) { 
      TreeSetIterator (tree, AddLoad);
      object_names = colorsd -> loads;
      UpdateList (colorsd -> llist, tree, deleted);
      colorsd -> loads = object_names;
   }
 
   else if (tree == problem.force_tree) {
      TreeSetIterator (tree, AddForce);
      object_names = colorsd -> forces;
      UpdateList (colorsd -> flist, tree, deleted);
      colorsd -> forces = object_names;
   } 

   for (i = 0 ; i < object_count ; i++)
      InsertColor (colorsd, object_colors [i]);
  
   return;
}

void ColorsDialogUpdateMaterialList (ColorsDialog colorsd, Problem::MaterialSet *tree, Boolean deleted)
{
   object_names = colorsd -> materials;
   UpdateList (colorsd -> mlist, tree, deleted, AddMaterial);
   colorsd -> materials = object_names;

   for (unsigned i = 0 ; i < object_count ; i++)
      InsertColor (colorsd, object_colors [i]);
  
   return;
}

/************************************************************************
 * Function:	ColorsDialogCreate					*
 *									*
 * Description:	Creates a new colors dialog.  You would never want to	*
 *		have more than one of these but the interface is kept	*
 *		consistent with those of the other dialogs.		*
 ************************************************************************/

ColorsDialog ColorsDialogCreate (Widget parent, String name, String title)
{
    Cardinal		i;
    Arg			args [1];
    Widget		group [8];
    ColorsDialog	colorsd;
    Dimension		width;
    Position		x;
    static XtAppContext	app_context = NULL;
    static XtActionsRec	actions [ ] = {{"ColorsDialogAction", Action}};


    /* Perform one time initialization. */

    if (app_context == NULL) {
	app_context = XtWidgetToApplicationContext (parent);
	XtAppAddActions (app_context, actions, XtNumber (actions));
	AddAutoRepeatAction (app_context);

        layout_args [0].value = StringToLayout (parent, layout_string);

	command_translations = XtParseTranslationTable (command_table);
	viewport_translations = XtParseTranslationTable (viewport_table);
	help_translations = XtParseTranslationTable (help_table);
    }


    /* Create the material dialog and its widgets. */

    XtSetArg (shell_args [0], XtNtitle, title);
    XtSetArg (shell_args [1], XtNiconName, title);

    colorsd = XtNew (struct colors_dialog);

    colorsd -> shell    = XtCreatePopupShell (name,
			 topLevelShellWidgetClass, parent,
			 shell_args, XtNumber (shell_args));

    colorsd -> layout   = XtCreateManagedWidget ("layout",
			 layoutWidgetClass, colorsd -> shell,
			 layout_args, XtNumber (layout_args));

    colorsd -> cview    = XtCreateManagedWidget ("cview",
			 viewportWidgetClass, colorsd -> layout,
			 viewport_args, XtNumber (viewport_args));

    colorsd -> mview    = XtCreateManagedWidget ("mview",
			 viewportWidgetClass, colorsd -> layout,
			 viewport_args, XtNumber (viewport_args));

    colorsd -> fview    = XtCreateManagedWidget ("fview",
			 viewportWidgetClass, colorsd -> layout,
			 viewport_args, XtNumber (viewport_args));

    colorsd -> lview    = XtCreateManagedWidget ("lview",
			 viewportWidgetClass, colorsd -> layout,
			 viewport_args, XtNumber (viewport_args));

    colorsd -> clist     = XtCreateManagedWidget ("clist",
			 listWidgetClass, colorsd -> cview,
			 list_args, XtNumber (list_args));

    colorsd -> mlist     = XtCreateManagedWidget ("mlist",
			 listWidgetClass, colorsd -> mview,
			 list_args, XtNumber (list_args));

    colorsd -> flist     = XtCreateManagedWidget ("flist",
			 listWidgetClass, colorsd -> fview,
			 list_args, XtNumber (list_args));

    colorsd -> llist     = XtCreateManagedWidget ("llist",
			 listWidgetClass, colorsd -> lview,
			 list_args, XtNumber (list_args));

    colorsd -> colorview = XtCreateManagedWidget ("colorview",
			 viewportWidgetClass, colorsd -> layout,
			 viewport_args, XtNumber (viewport_args));

    colorsd -> colorlist = XtCreateManagedWidget ("colorlist",
			 listWidgetClass, colorsd -> colorview,
			 colorlist_args, XtNumber (colorlist_args));

    colorsd -> recolor  = XtCreateManagedWidget ("recolor",
			 commandWidgetClass, colorsd -> layout,
			 NULL, 0);

    colorsd -> dismiss  = XtCreateManagedWidget ("dismiss",
			 commandWidgetClass, colorsd -> layout,
			 NULL, 0);

    colorsd -> help     = CreateHelpButton (colorsd -> layout, "help");

    for (i = 0; i < XtNumber (labels); i ++) {
	label_args [0].value = (XtArgVal) labels [i];
	XtCreateManagedWidget (names [i], labelWidgetClass,
		colorsd -> layout, label_args, XtNumber (label_args));
    }


    XtCreateManagedWidget ("separator1", coreWidgetClass,
			colorsd -> layout, core_args, XtNumber (core_args));

    XtCreateManagedWidget ("separator2", coreWidgetClass,
			colorsd -> layout, core_args, XtNumber (core_args));


    /* Create a tab group for the colors dialog. */

    group [0]  = colorsd -> mview;
    group [1]  = colorsd -> lview;
    group [2]  = colorsd -> cview;
    group [3]  = colorsd -> fview;
    group [4]  = colorsd -> colorview;
    group [5]  = colorsd -> help;
    group [6]  = colorsd -> recolor;
    group [7]  = colorsd -> dismiss;

    XtGetValues (colorsd -> layout, color_args, XtNumber (color_args));
    CreateTabGroup (colorsd -> shell, group, XtNumber (group), 
                    highlight, True);

    XtRealizeWidget (colorsd -> shell);
    SetFocus (colorsd -> mview);

    XtSetArg (args [0], XtNwidth, &width);
    XtGetValues (colorsd -> layout, args, 1);
    XtSetArg (args [0], XtNx, &x);
    XtGetValues (colorsd -> help, args, 1);
    UpdateHelpMessage (colorsd -> help, help_message, width - 2 * x);


    /* Define the initial colors array.	*/

    colorsd -> numcolors = XtNumber (default_colors);
    colorsd -> colors = (String *) XtMalloc (colorsd -> numcolors * sizeof (String));

    for (i = 0 ; i < colorsd -> numcolors - 1 ; i++) 
       colorsd -> colors [i] = XtNewString (default_colors [i]);

    colorsd -> colors [colorsd -> numcolors - 1] = NULL;

    colorsd -> materials   = NULL;
    colorsd -> forces      = NULL;
    colorsd -> constraints = NULL;
    colorsd -> loads       = NULL;

    /* Add the translations to each widget. */

    AddDeleteWindowProtocol   (colorsd -> shell, "ColorsDialogAction()");
    ListAddCursorTranslations (colorsd -> mview);
    ListAddCursorTranslations (colorsd -> cview);
    ListAddCursorTranslations (colorsd -> lview);
    ListAddCursorTranslations (colorsd -> fview);
    ListAddCursorTranslations (colorsd -> colorview);

    XtOverrideTranslations (colorsd -> mview,	viewport_translations);
    XtOverrideTranslations (colorsd -> cview,	viewport_translations);
    XtOverrideTranslations (colorsd -> fview,	viewport_translations);
    XtOverrideTranslations (colorsd -> lview,	viewport_translations);
    XtOverrideTranslations (colorsd -> colorview, viewport_translations);

    XtOverrideTranslations (colorsd -> recolor, command_translations);
    XtOverrideTranslations (colorsd -> dismiss, command_translations);
    XtOverrideTranslations (colorsd -> help, help_translations);


    /* Add the necessary callbacks. */

    XtAddCallback(colorsd -> mlist, XtNcallback, ObjectChange, (XtPointer) colorsd);
    XtAddCallback(colorsd -> clist, XtNcallback, ObjectChange, (XtPointer) colorsd);
    XtAddCallback(colorsd -> flist, XtNcallback, ObjectChange, (XtPointer) colorsd);
    XtAddCallback(colorsd -> llist, XtNcallback, ObjectChange, (XtPointer) colorsd);
    XtAddCallback(colorsd -> colorlist, XtNcallback, ColorChange, (XtPointer) colorsd);

    XtAddCallback(colorsd -> recolor, XtNcallback, ExternalCallback, (XtPointer) colorsd);
    XtAddCallback(colorsd -> dismiss, XtNcallback, Dismiss, (XtPointer) colorsd);

    return colorsd;
}


/************************************************************************
 * Function:	ColorsDialogPopup					*
 *									*
 * Description:	Pops up the specified material dialog.			*
 ************************************************************************/

void ColorsDialogPopup (ColorsDialog colorsd)
{
    XtPopup (colorsd -> shell, XtGrabNone);
}
