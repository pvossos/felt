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
 * File:	material.c						*
 *									*
 * Description:	This file contains the public and private function and	*
 *		type definitions for the material dialog box.		*
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
# include "Layout.h"
# include "Material.h"
# include "TabGroup.h"
# include "util.h"
# include "fe.h"
# include "objects.h"

# ifndef X_NOT_STDC_ENV
# include <stdlib.h>
# endif

# ifndef XtNshadowWidth
# define XtNshadowWidth "shadowWidth"
# endif


struct material_dialog {
    Widget         shell;	/* topLevelShell  <specified>	*/
    Widget         layout;	/*	Layout  layout		*/
    Widget         name;	/*	     AsciiText  name	*/
    Widget         E;		/*	     AsciiText  E	*/
    Widget         A;		/*	     AsciiText  A	*/
    Widget         nu_;		/*	     AsciiText  nu	*/
    Widget         t;		/*	     AsciiText  t	*/
    Widget         Ix;		/*	     AsciiText  Ix	*/
    Widget         Iy;		/*	     AsciiText  Iy	*/
    Widget         Iz;		/*	     AsciiText  Iz	*/
    Widget         J;		/*	     AsciiText  J	*/
    Widget         G;		/*	     AsciiText  G	*/
    Widget         kappa;	/*	     AsciiText  kappa	*/
    Widget         rho;		/*	     AsciiText  rho	*/
    Widget         Rk;		/*	     AsciiText  Rk	*/
    Widget	   Rm;		/*	     AsciiText  Rm      */
    Widget         Kx;		/*	     AsciiText  Kx	*/
    Widget         Ky;		/*	     AsciiText  Ky	*/
    Widget         Kz;		/*	     AsciiText  Kz	*/
    Widget	   c;           /*           AsciiText  c	*/
    Widget	   Y;		/*	     AsciiText  Y	*/
    Widget         viewport;	/*	     Viewport  viewport	*/
    Widget         list;	/*		  List  list	*/
    Widget         help;	/*	     MenuButton  help	*/
    Widget         accept;	/*	     Command  accept	*/
    Widget         dismiss;	/*	     Command  dismiss	*/
    Widget         nuke;	/*	     Command  delete	*/
    Widget         nu;		/*	     Command  new	*/
    Widget         copy;	/*	     Command  copy	*/
    XtCallbackProc callback;
    XtPointer	   closure;
    String        *materials;
    Material       active;
    Boolean        new_copy;
    Tree           tree;
};

static String labels [ ] = {
    "Name:"
};

static String names [ ] = {
    "nameLabel"
};

static String property_labels [ ] = {
    "E:", "A:", "n:", "t:", "Ix:", "Iy:", "Iz:", "J:", "G:", "k:", "r:", 
    "Rk:", "Rm:", "Kx:", "Ky:", "Kz:", "c:", "Y:"
};

static String property_names [ ] = {
    "E_name", "A_name", "nu_name", "t_name", "Ix_name", "Iy_name", 
    "Iz_name", "J_name", "G_name", "kappa_name", "rho_name", "Rk_name",
    "Rm_name", "Kx_name", "Ky_name", "Kz_name", "c_name", "Y_name"
};

static String property_help [ ] = {
    "Young's (elastic) modulus: everything",
    "cross-sectional area: beam, beam3d, truss",
    "Poisson's ratio: planar and solid elements",
    "thickness: 2d planar elements",
    "moment of inertia about x-x axis: beam",
    "moment of inertia about y-y axis: beam3d",
    "moment of inertia about z-z axis: beam3d",
    "polar moment of inertia: beam3d",
    "bulk (shear) modulus: beam3d",
    "shear correction factor in Timoshenko beam theory",
    "density: used to determine weights and form mass matrices",
    "Rayleigh damping coefficient (stiffness)",
    "Rayleigh damping coefficient (mass)",
    "thermal conductivity along the x-direction",
    "thermal conductivity along the y-direction",
    "thermal conductivity along the z-direction",
    "heat capacitance",
    "Yield strength (never used!)",
};


static MaterialDialog  dialog;
static Cardinal	       num_materials;
static int	       list_index;


/* Resources */

static Pixel highlight;

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
	    ((height E - height E_name) / 2) \
            E_name \
	    ((height E - height E_name) / 2) \
	    4 \
            ((height A - height A_name) / 2) \
            A_name \
            ((height A - height A_name) / 2) \
	    4  \
            ((height nu - height nu_name) / 2) \
            nu_name \
            ((height nu - height nu_name) / 2) \
	    4  \
            ((height t - height t) / 2) \
            t_name \
            ((height t - height t) / 2) \
	    4  \
            ((height kappa - height kappa_name) / 2) \
            kappa_name \
            ((height kappa - height kappa_name) / 2) \
	    4 \
            ((height rho - height rho_name) / 2) \
            rho_name \
            ((height rho - height rho_name) / 2) \
	    4 \
            ((height Rk - height Rk_name) /2) \
            Rk_name \
            ((height Rk - height Rk_name) /2) \
	    4 \
            ((height Rm - height Rm_name) /2) \
            Rm_name \
            ((height Rm - height Rm_name) /2) \
	    4 \
            ((height Y - height Y_name) / 2) \
            Y_name \
            ((height Y - height Y_name) / 2) \
	    4 \
 	} \
 	1 \
 	vertical { \
 	    4 \
            E \
	    4 \
            A \
	    4 \
            nu \
	    4 \
            t\
 	    4 \
            kappa \
	    4 \
            rho \
	    4 \
            Rk \
 	    4 \
            Rm \
            4 \
            Y \
            4 \
 	} \
 	6 \
 	vertical { \
 	    4 \
            ((height Ix - height Ix_name) /2) \
            Ix_name \
            ((height Ix - height Ix_name) /2) \
	    4 \
            ((height Iy - height Iy_name) /2) \
            Iy_name \
            ((height Iy - height Iy_name) /2) \
	    4 \
            ((height Iz - height Iz_name) /2) \
            Iz_name \
            ((height Iz - height Iz_name) /2) \
	    4 \
            ((height J - height J_name) /2) \
            J_name \
            ((height J - height J_name) /2) \
	    4 \
            ((height G - height G_name) /2) \
            G_name \
            ((height G - height G_name) /2) \
	    4 \
            ((height Kx - height Kx_name) /2) \
            Kx_name \
            ((height Kx - height Kx_name) /2) \
            4 \
            ((height Ky - height Ky_name) /2) \
            Ky_name \
            ((height Ky - height Ky_name) /2) \
            4 \
            ((height Kz - height Kz_name) /2) \
            Kz_name \
            ((height Kz - height Kz_name) /2) \
            4 \
            ((height c - height c_name) / 2) \
            c_name \
            ((height c - height c_name) / 2) \
	    4 \
 	} \
 	1 \
 	vertical { \
 	    4 \
            Ix \
	    4 \
            Iy \
	    4 \
            Iz \
	    4 \
            J \
 	    4 \
            G \
	    4 \
            Kx \
	    4 \
            Ky \
	    4 \
            Kz \
	    4 \
            c \
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

static String dummy_list [ ] = {
    NULL
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

static Arg property_args [ ] = {
    {XtNlabel,	            (XtArgVal) ""},
    {XtNborderWidth,        (XtArgVal) 0},
    {XtNhighlightThickness, (XtArgVal) 0},
    {XtNshadowWidth,	    (XtArgVal) 0},
};

static Arg core_args [ ] = {
    {XtNwidth,  (XtArgVal) 3},
    {XtNheight, (XtArgVal) 3},
};


/* Translation tables */

static String text_table =
"<Key>Return: MaterialDialogAction(accept)\n\
 <Key>Escape: MaterialDialogAction(dismiss)\n\
 Ctrl<Key>d:  MaterialDialogAction(delete)\n\
 Ctrl<Key>c:  MaterialDialogAction(copy)\n\
 Ctrl<Key>n:  MaterialDialogAction(new)\n\
 Ctrl<Key>h:  MaterialDialogAction(help)\n\
 <Btn1Down>:  SetFocus() select-start()";

static XtTranslations text_translations;


static String command_table =
"<Key>Return:  MaterialDialogAction(accept)\n\
 <Key>Escape:  MaterialDialogAction(dismiss)\n\
 Ctrl<Key>d:   MaterialDialogAction(delete)\n\
 Ctrl<Key>c:   MaterialDialogAction(copy)\n\
 Ctrl<Key>n:   MaterialDialogAction(new)\n\
 Ctrl<Key>h:   MaterialDialogAction(help)\n\
 <Key>space:   AutoRepeat(off) set()\n\
 <KeyUp>space: AutoRepeat(saved) notify() unset()";

static XtTranslations command_translations;


static String viewport_table =
"<Key>Return: MaterialDialogAction(accept)\n\
 <Key>Escape: MaterialDialogAction(dismiss)\n\
 Ctrl<Key>d:  MaterialDialogAction(delete)\n\
 Ctrl<Key>c:  MaterialDialogAction(copy)\n\
 Ctrl<Key>n:  MaterialDialogAction(new)\n\
 Ctrl<Key>h:  MaterialDialogAction(help)\n\
 <Btn1Down>:  SetFocus()";

static XtTranslations viewport_translations;


static String help_table =
"<Key>Return: MaterialDialogAction(accept)\n\
 <Key>Escape: MaterialDialogAction(dismiss)\n\
 Ctrl<Key>d:  MaterialDialogAction(delete)\n\
 Ctrl<Key>c:  MaterialDialogAction(copy)\n\
 Ctrl<Key>n:  MaterialDialogAction(new)\n\
 Ctrl<Key>h:  MaterialDialogAction(help)\n\
 <Key>space:  PostMenu()";

static XtTranslations help_translations;


/* Help message. */

static String help_message ="\
Use the material form to define, edit and delete materials.  The name box \
displays the name of the current material property and can be used to name \
(or rename) a material.  Use the list to change the current \
material.  The text fields to the right define the properties of the current \
material.  Clicking on a property name displays a brief description of that \
property.  Use 'Accept' to register your changes. 'Delete' erases the current \
force. 'New' empties all fields. 'Copy' empties the name field only."; 


/************************************************************************
 * Function:	AppendMaterialName				 	*	
 *									*
 * Description:	Appends the material name to the array of names.  The	*
 *		index of the active material is also set.		*
 ************************************************************************/

static int AppendMaterialName (Item item)
{
    if (dialog -> active == (Material) item)
	list_index = num_materials;

    dialog -> materials [num_materials ++] = XtNewString(((Material) item) -> name.c_str());
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
 *		selected material if the widget is not null, or the 	*
 *		active material if the widget is null.  The newly 	*
 *		displayed material is made the active material and any 	*
 *		new/copy operation is canceled.				*
 ************************************************************************/

static void Change (Widget w, XtPointer client_data, XtPointer call_data)
{
    char		 buffer [32];
    Material		 active;
    struct material	 dummy;
    MaterialDialog	 materiald;
    XawListReturnStruct	*info;


    materiald = (MaterialDialog) client_data;


    /* Retrieve the active material from the tree if selected. */

    if (w != NULL) {
	info = (XawListReturnStruct *) call_data;
	if (info -> list_index == XAW_LIST_NONE)
	    return;

	dummy.name = info -> string;
	materiald -> active = (Material) TreeSearch (materiald -> tree, &dummy);
    }

    active = materiald -> active;
    materiald -> new_copy = False;


    /* Update all of the text entries. */

    SetTextString (materiald -> name, active -> name.c_str());

    sprintf (buffer, (active -> E ? "%g" : ""), active -> E);
    SetTextString (materiald -> E, buffer);

    sprintf (buffer, (active -> A ? "%g" : ""), active -> A);
    SetTextString (materiald -> A, buffer);

    sprintf (buffer, (active -> nu ? "%g" : ""), active -> nu);
    SetTextString (materiald -> nu_, buffer);

    sprintf (buffer, (active -> t ? "%g" : ""), active -> t);
    SetTextString (materiald -> t, buffer);

    sprintf (buffer, (active -> Ix ? "%g" : ""), active -> Ix);
    SetTextString (materiald -> Ix, buffer);

    sprintf (buffer, (active -> Iy ? "%g" : ""), active -> Iy);
    SetTextString (materiald -> Iy, buffer);

    sprintf (buffer, (active -> Iz ? "%g" : ""), active -> Iz);
    SetTextString (materiald -> Iz, buffer);

    sprintf (buffer, (active -> J ? "%g" : ""), active -> J);
    SetTextString (materiald -> J, buffer);

    sprintf (buffer, (active -> G ? "%g" : ""), active -> G);
    SetTextString (materiald -> G, buffer);

    sprintf (buffer, (active -> kappa ? "%g" : ""), active -> kappa);
    SetTextString (materiald -> kappa, buffer);

    sprintf (buffer, (active -> rho ? "%g" : ""), active -> rho);
    SetTextString (materiald -> rho, buffer);

    sprintf (buffer, (active -> Rk ? "%g" : ""), active -> Rk);
    SetTextString (materiald -> Rk, buffer);

    sprintf (buffer, (active -> Rm ? "%g" : ""), active -> Rm);
    SetTextString (materiald -> Rm, buffer);

    sprintf (buffer, (active -> Kx ? "%g" : ""), active -> Kx);
    SetTextString (materiald -> Kx, buffer);

    sprintf (buffer, (active -> Ky ? "%g" : ""), active -> Ky);
    SetTextString (materiald -> Ky, buffer);

    sprintf (buffer, (active -> Kz ? "%g" : ""), active -> Kz);
    SetTextString (materiald -> Kz, buffer);

    sprintf (buffer, (active -> c ? "%g" : ""), active -> c);
    SetTextString (materiald -> c, buffer);


}


/************************************************************************
 * Function:	Accept							*
 *									*
 * Description:	Accepts changes made to currently displayed material.	*
 *		If the name is empty or a duplicate name is given then	*
 *		an error is reported.  Otherwise, a new material is	*
 *		created if a new/copy operation is in effect.  The	*
 *		material is then redisplayed to correct any invalid	*
 *		entries.						*
 ************************************************************************/

static void Accept (Widget w, XtPointer client_data, XtPointer call_data)
{
    struct material    old;
    struct material    dummy;
    Material	       found;
    Material	       active;
    Boolean	       duplicate;
    MaterialDialog     materiald;
    MaterialDialogInfo info;


    materiald = (MaterialDialog) client_data;


    /* Retrieve the name of the material. */

    dummy.name = GetTextString (materiald -> name);
    found = (Material) TreeSearch (materiald -> tree, &dummy);
    duplicate=found && (found != materiald -> active || materiald -> new_copy);


    /* Check for a duplicate name. */

    if (!dummy.name [0] || duplicate) {
	XBell (XtDisplay (materiald -> name), 0);
	SetFocus (materiald -> name);
	if (!materiald -> new_copy)
	    SetTextString (materiald -> name, materiald -> active -> name.c_str());
	else
	    SetTextString (materiald -> name, "");

    } else {


	/* Create a new material or new name as needed. */

	if (materiald -> new_copy)
	    materiald -> active = CreateMaterial (dummy.name.c_str());
	else if (strcmp (materiald -> active -> name.c_str(), dummy.name.c_str())) {
        old.name = materiald -> active -> name;
        TreeDelete (materiald -> tree, &old);
        materiald->active->name = dummy.name;
        TreeInsert (materiald -> tree, materiald -> active);
	}

	active = materiald -> active;
	active -> E	= exptod (GetTextString (materiald -> E), NULL);
	active -> A	= exptod (GetTextString (materiald -> A), NULL);
	active -> nu	= exptod (GetTextString (materiald -> nu_), NULL);
	active -> t	= exptod (GetTextString (materiald -> t), NULL);
	active -> Ix	= exptod (GetTextString (materiald -> Ix), NULL);
	active -> Iy	= exptod (GetTextString (materiald -> Iy), NULL);
	active -> Iz	= exptod (GetTextString (materiald -> Iz), NULL);
	active -> J	= exptod (GetTextString (materiald -> J), NULL);
	active -> G	= exptod (GetTextString (materiald -> G), NULL);
	active -> kappa	= exptod (GetTextString (materiald -> kappa), NULL);
	active -> rho	= exptod (GetTextString (materiald -> rho), NULL);
	active -> Rk	= exptod (GetTextString (materiald -> Rk), NULL);
	active -> Rm	= exptod (GetTextString (materiald -> Rm), NULL);
	active -> Kx	= exptod (GetTextString (materiald -> Kx), NULL);
	active -> Ky	= exptod (GetTextString (materiald -> Ky), NULL);
	active -> Kz	= exptod (GetTextString (materiald -> Kz), NULL);
	active -> c	= exptod (GetTextString (materiald -> c), NULL);

	if (materiald -> new_copy)
	    TreeInsert (materiald -> tree, materiald -> active);

	if (materiald -> callback != NULL) {
	    w = materiald -> shell;
	    info.dialog   = materiald;
	    info.material = materiald -> active;
	    info.deleted  = False;
	    info.proceed  = True;
	    materiald -> callback (w, materiald -> closure, &info);
	}

	MaterialDialogUpdate (materiald, materiald -> tree);
    }
}


/************************************************************************
 * Function:	Dismiss							*
 *									*
 * Description:	Pops down the dialog box.				*
 ************************************************************************/

static void Dismiss (Widget w, XtPointer client_data, XtPointer call_data)
{
    MaterialDialog materiald;


    materiald = (MaterialDialog) client_data;
    XtPopdown (materiald -> shell);
}


/************************************************************************
 * Function:	Delete							*
 *									*
 * Description:	Deletes the active material if a new/copy operation is  *
 *		not in effect.  The dialog is then updated.		*
 ************************************************************************/

static void Delete (Widget w, XtPointer client_data, XtPointer call_data)
{
    MaterialDialog     materiald;
    MaterialDialogInfo info;


    materiald = (MaterialDialog) client_data;

    if (!materiald -> new_copy) {
	if (materiald -> callback != NULL) {
	    w = materiald -> shell;
	    info.dialog   = materiald;
	    info.material = materiald -> active;
	    info.deleted  = True;
	    info.proceed  = True;
	    materiald -> callback (w, materiald -> closure, &info);
	    if (info.proceed == False)
		return;
	}

	TreeDelete (materiald -> tree, materiald -> active);
	DestroyMaterial (materiald -> active);
	materiald -> active = NULL;
    }

    MaterialDialogUpdate (materiald, materiald -> tree);
}


/************************************************************************
 * Function:	Copy							*
 *									*
 * Description:	Clears the name entry only and sets the flag indicating	*
 *		that a new/copy operation is in effect.			*
 ************************************************************************/

static void Copy (Widget w, XtPointer client_data, XtPointer call_data)
{
    MaterialDialog materiald;


    materiald = (MaterialDialog) client_data;

    materiald -> new_copy = True;
    SetFocus (materiald -> name);
    XawListUnhighlight (materiald -> list);
    SetTextString (materiald -> name, "");
}


/************************************************************************
 * Function:	New							*
 *									*
 * Description:	Clears all entries in the material dialog and sets the	*
 *		flag indicating that a new/copy operation is in effect.	*
 ************************************************************************/

static void New (Widget w, XtPointer client_data, XtPointer call_data)
{
    MaterialDialog materiald;


    materiald = (MaterialDialog) client_data;

    Copy (NULL, client_data, NULL);
    SetTextString (materiald -> E, "");
    SetTextString (materiald -> A, "");
    SetTextString (materiald -> nu_, "");
    SetTextString (materiald -> t, "");
    SetTextString (materiald -> Ix, "");
    SetTextString (materiald -> Iy, "");
    SetTextString (materiald -> Iz, "");
    SetTextString (materiald -> J, "");
    SetTextString (materiald -> G, "");
    SetTextString (materiald -> rho, "");
    SetTextString (materiald -> kappa, "");
    SetTextString (materiald -> Rk, "");
    SetTextString (materiald -> Rm, "");
    SetTextString (materiald -> Kx, "");
    SetTextString (materiald -> Ky, "");
    SetTextString (materiald -> Kz, "");
    SetTextString (materiald -> c, "");
}


/************************************************************************
 * Function:	MaterialDialogCreate					*
 *									*
 * Description:	Creates a new material dialog.  You would never want to	*
 *		have more than one of these but the interface is kept	*
 *		consistent with those of the other dialogs.		*
 ************************************************************************/

MaterialDialog MaterialDialogCreate (Widget parent, String name, String title, XtCallbackProc callback, XtPointer closure)
{
    Cardinal		i;
    Arg			args [1];
    Widget		group [26];
    MaterialDialog	materiald;
    Dimension		width;
    Position		x;
    Widget		property_help_widget;
    static XtAppContext	app_context = NULL;
    static XtActionsRec	actions [ ] = {{"MaterialDialogAction", Action}};


    /* Perform one time initialization. */

    if (app_context == NULL) {
	app_context = XtWidgetToApplicationContext (parent);
	XtAppAddActions (app_context, actions, XtNumber (actions));
	AddAutoRepeatAction (app_context);

        layout_args [0].value = StringToLayout (parent, layout_string);

	text_translations = XtParseTranslationTable (text_table);
	command_translations = XtParseTranslationTable (command_table);
	viewport_translations = XtParseTranslationTable (viewport_table);
	help_translations = XtParseTranslationTable (help_table);
    }


    /* Create the material dialog and its widgets. */

    XtSetArg (shell_args [0], XtNtitle, title);
    XtSetArg (shell_args [1], XtNiconName, title);

    materiald = XtNew (struct material_dialog);

    materiald -> callback = callback;

    materiald -> closure  = closure;

    materiald -> materials   = NULL;

    materiald -> active   = NULL;

    materiald -> shell    = XtCreatePopupShell (name,
			 topLevelShellWidgetClass, parent,
			 shell_args, XtNumber (shell_args));

    materiald -> layout   = XtCreateManagedWidget ("layout",
			 layoutWidgetClass, materiald -> shell,
			 layout_args, XtNumber (layout_args));

    materiald -> viewport = XtCreateManagedWidget ("viewport",
			 viewportWidgetClass, materiald -> layout,
			 viewport_args, XtNumber (viewport_args));

    materiald -> list     = XtCreateManagedWidget ("list",
			 listWidgetClass, materiald -> viewport,
			 list_args, XtNumber (list_args));

    materiald -> name     = XtCreateManagedWidget ("name",
			 asciiTextWidgetClass, materiald -> layout,
			 text_args, XtNumber (text_args));

    materiald -> E  = XtCreateManagedWidget ("E",
			 asciiTextWidgetClass, materiald -> layout,
			 text_args, XtNumber (text_args));

    materiald -> A  = XtCreateManagedWidget ("A",
			 asciiTextWidgetClass, materiald -> layout,
			 text_args, XtNumber (text_args));

    materiald -> nu_  = XtCreateManagedWidget ("nu",
			 asciiTextWidgetClass, materiald -> layout,
			 text_args, XtNumber (text_args));

    materiald -> t = XtCreateManagedWidget ("t",
			 asciiTextWidgetClass, materiald -> layout,
			 text_args, XtNumber (text_args));

    materiald -> Ix = XtCreateManagedWidget ("Ix",
			 asciiTextWidgetClass, materiald -> layout,
			 text_args, XtNumber (text_args));

    materiald -> Iy = XtCreateManagedWidget ("Iy",
			 asciiTextWidgetClass, materiald -> layout,
			 text_args, XtNumber (text_args));

    materiald -> Iz = XtCreateManagedWidget ("Iz",
			 asciiTextWidgetClass, materiald -> layout,
			 text_args, XtNumber (text_args));

    materiald -> J = XtCreateManagedWidget ("J",
			 asciiTextWidgetClass, materiald -> layout,
			 text_args, XtNumber (text_args));

    materiald -> G = XtCreateManagedWidget ("G",
			 asciiTextWidgetClass, materiald -> layout,
			 text_args, XtNumber (text_args));

    materiald -> kappa = XtCreateManagedWidget ("kappa",
			 asciiTextWidgetClass, materiald -> layout,
			 text_args, XtNumber (text_args));

    materiald -> rho = XtCreateManagedWidget ("rho",
			 asciiTextWidgetClass, materiald -> layout,
			 text_args, XtNumber (text_args));

    materiald -> Rk = XtCreateManagedWidget ("Rk",
			 asciiTextWidgetClass, materiald -> layout,
			 text_args, XtNumber (text_args));

    materiald -> Rm = XtCreateManagedWidget ("Rm",
			 asciiTextWidgetClass, materiald -> layout,
			 text_args, XtNumber (text_args));

    materiald -> Y = XtCreateManagedWidget ("Y",
			 asciiTextWidgetClass, materiald -> layout,
			 text_args, XtNumber (text_args));

    materiald -> Kx = XtCreateManagedWidget ("Kx",
			 asciiTextWidgetClass, materiald -> layout,
			 text_args, XtNumber (text_args));

    materiald -> Ky = XtCreateManagedWidget ("Ky",
			 asciiTextWidgetClass, materiald -> layout,
			 text_args, XtNumber (text_args));

    materiald -> Kz = XtCreateManagedWidget ("Kz",
			 asciiTextWidgetClass, materiald -> layout,
			 text_args, XtNumber (text_args));

    materiald -> c = XtCreateManagedWidget ("c",
			 asciiTextWidgetClass, materiald -> layout,
			 text_args, XtNumber (text_args));

    materiald -> accept   = XtCreateManagedWidget ("accept",
			 commandWidgetClass, materiald -> layout,
			 NULL, 0);

    materiald -> dismiss  = XtCreateManagedWidget ("dismiss",
			 commandWidgetClass, materiald -> layout,
			 NULL, 0);

    materiald -> nuke   = XtCreateManagedWidget ("delete",
			 commandWidgetClass, materiald -> layout,
			 NULL, 0);

    materiald -> nu      = XtCreateManagedWidget ("new",
			 commandWidgetClass, materiald -> layout,
			 NULL, 0);

    materiald -> copy     = XtCreateManagedWidget ("copy",
			 commandWidgetClass, materiald -> layout,
			 NULL, 0);

    materiald -> help     = CreateHelpButton (materiald -> layout, "help");

    for (i = 0; i < XtNumber (labels); i ++) {
	label_args [0].value = (XtArgVal) labels [i];
	XtCreateManagedWidget (names [i], labelWidgetClass,
		materiald -> layout, label_args, XtNumber (label_args));
    }

    for (i = 0 ; i < XtNumber (property_labels) ; i++) {
        property_args [0].value = (XtArgVal) property_labels [i];
        property_help_widget = CreateHelpButton (materiald -> layout, 
                                                 property_names [i]);

        XtSetValues (property_help_widget, property_args, 
                     XtNumber (property_args));

        UpdateHelpMessage (property_help_widget, property_help [i], 200);
    }


    XtCreateManagedWidget ("separator1", coreWidgetClass,
			materiald -> layout, core_args, XtNumber (core_args));

    XtCreateManagedWidget ("separator2", coreWidgetClass,
			materiald -> layout, core_args, XtNumber (core_args));


    /* Create a tab group for the material dialog. */

    i = 0;
    group [i++]  = materiald -> name;
    group [i++]  = materiald -> viewport;
    group [i++]  = materiald -> E;
    group [i++]  = materiald -> A;
    group [i++]  = materiald -> nu_;
    group [i++]  = materiald -> t;
    group [i++]  = materiald -> kappa;
    group [i++]  = materiald -> rho;
    group [i++]  = materiald -> Rk;
    group [i++]  = materiald -> Rm;
    group [i++]  = materiald -> Y;
    group [i++]  = materiald -> Ix;
    group [i++]  = materiald -> Iy;
    group [i++]  = materiald -> Iz;
    group [i++]  = materiald -> J;
    group [i++]  = materiald -> G;
    group [i++]  = materiald -> Kx;
    group [i++]  = materiald -> Ky;
    group [i++]  = materiald -> Kz;
    group [i++]  = materiald -> c;
    group [i++]  = materiald -> help;
    group [i++]  = materiald -> accept;
    group [i++] = materiald -> dismiss;
    group [i++] = materiald -> nuke;
    group [i++] = materiald -> nu;
    group [i++] = materiald -> copy;

    XtSetSensitive (materiald -> Y, False);

    XtGetValues (materiald -> layout, color_args, XtNumber (color_args));
    CreateTabGroup (materiald -> shell, group, XtNumber (group), 
                    highlight, True);

    XtRealizeWidget (materiald -> shell);
    SetFocus (materiald -> name);

    XtSetArg (args [0], XtNwidth, &width);
    XtGetValues (materiald -> layout, args, 1);
    XtSetArg (args [0], XtNx, &x);
    XtGetValues (materiald -> help, args, 1);
    UpdateHelpMessage (materiald -> help, help_message, width - 2 * x);


    /* Add the translations to each widget. */

    AddDeleteWindowProtocol   (materiald -> shell, "MaterialDialogAction()");
    ListAddCursorTranslations (materiald -> viewport);

    XtOverrideTranslations (materiald -> name,	text_translations);
    XtOverrideTranslations (materiald -> E,	text_translations);
    XtOverrideTranslations (materiald -> A,	text_translations);
    XtOverrideTranslations (materiald -> nu_,	text_translations);
    XtOverrideTranslations (materiald -> t,	text_translations);
    XtOverrideTranslations (materiald -> Ix,	text_translations);
    XtOverrideTranslations (materiald -> Iy,	text_translations);
    XtOverrideTranslations (materiald -> Iz,	text_translations);
    XtOverrideTranslations (materiald -> J,	text_translations);
    XtOverrideTranslations (materiald -> G,	text_translations);
    XtOverrideTranslations (materiald -> kappa,	text_translations);
    XtOverrideTranslations (materiald -> rho,	text_translations);
    XtOverrideTranslations (materiald -> Rk,	text_translations);
    XtOverrideTranslations (materiald -> Rm,	text_translations);
    XtOverrideTranslations (materiald -> Y,	text_translations);
    XtOverrideTranslations (materiald -> Kx,	text_translations);
    XtOverrideTranslations (materiald -> Ky,	text_translations);
    XtOverrideTranslations (materiald -> Kz,	text_translations);
    XtOverrideTranslations (materiald -> c,	text_translations);
    XtOverrideTranslations (materiald -> dismiss,command_translations);
    XtOverrideTranslations (materiald -> accept, command_translations);
    XtOverrideTranslations (materiald -> nuke, command_translations);
    XtOverrideTranslations (materiald -> nu, 	 command_translations);
    XtOverrideTranslations (materiald -> copy,	 command_translations);
    XtOverrideTranslations (materiald -> viewport,	viewport_translations);
    XtOverrideTranslations (materiald -> help, help_translations);


    /* Add the necessary callbacks. */

    XtAddCallback(materiald->list,   XtNcallback,Change, (XtPointer) materiald);
    XtAddCallback(materiald->accept, XtNcallback,Accept, (XtPointer) materiald);
    XtAddCallback(materiald->dismiss,XtNcallback,Dismiss,(XtPointer) materiald);
    XtAddCallback(materiald->nuke, XtNcallback,Delete, (XtPointer) materiald);
    XtAddCallback(materiald->nu,    XtNcallback,New,    (XtPointer) materiald);
    XtAddCallback(materiald->copy,   XtNcallback,Copy,   (XtPointer) materiald);

    return materiald;
}


/************************************************************************
 * Function:	MaterialDialogPopup					*
 *									*
 * Description:	Pops up the specified material dialog.			*
 ************************************************************************/

void MaterialDialogPopup (MaterialDialog materiald)
{
    XtPopup (materiald -> shell, XtGrabNone);
}


/************************************************************************
 * Function:	MaterialDialogActive					*
 *									*
 * Description:	Returns the currently active material.			*
 ************************************************************************/

Material MaterialDialogActive (MaterialDialog materiald)
{
    return materiald -> active;
}


/************************************************************************
 * Function:	MaterialDialogDisplay					*
 *									*
 * Description:	Displays a specified material.				*
 ************************************************************************/

void MaterialDialogDisplay (MaterialDialog materiald, Material material)
{
    materiald -> active = material;
    MaterialDialogUpdate (materiald, materiald -> tree);
}


/************************************************************************
 * Function:	MaterialDialogUpdate					*
 *									*
 * Description:	Updates the specified mat'l dialog with the specified	*
 *		tree. If no active material exists, the 1st material is	*
 *		made active.  If no active material still exists then	*
 *		a new operation is performed.  Otherwise a change	*
 *		operation is performed to display the active values.	*
 ************************************************************************/

void MaterialDialogUpdate (MaterialDialog materiald, Tree tree)
{
    Cardinal nbytes;


    /* Determine a new active material if necessary. */

    if (tree == NULL)
	tree = materiald -> tree;

    if (materiald -> active == NULL || tree != materiald -> tree)
	materiald -> active = (Material) TreeMinimum (tree);


    /* Construct the array of material names. */

    num_materials = 0;
    list_index = -1;
    dialog = materiald;
    materiald -> tree = tree;
    materiald -> new_copy = False;

    nbytes = (TreeSize (materiald -> tree) + 1) * sizeof (String);
    materiald -> materials = 
             (String *) XtRealloc ((char *) materiald -> materials, nbytes);

    TreeSetIterator (materiald -> tree, AppendMaterialName);
    TreeIterate (materiald -> tree);
    materiald -> materials [num_materials] = NULL;


    /* Update the list widget. */

    XawListChange (materiald -> list, materiald -> materials, 0, 0, True);

    if (list_index >= 0)
	XawListHighlight (materiald -> list, list_index);


    /* Update the text entries. */

    if (materiald -> active == NULL)
	New (NULL, (XtPointer) materiald, NULL);
    else
	Change (NULL, (XtPointer) materiald, NULL);
}
