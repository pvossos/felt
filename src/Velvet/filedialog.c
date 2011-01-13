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
 * File:	filedialog.c						*
 *									*
 * Description: This file contains the private and public function and	*
 *		type definitions for the file file dialog box.		*
 ************************************************************************/

# include <stdio.h>
# include <X11/Xos.h>
# include <sys/stat.h>
# include <pwd.h>
# include <dirent.h>
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
# include "FileDialog.h"
# include "TabGroup.h"
# include "scroll.h"
# include "util.h"

# ifndef X_NOT_STDC_ENV
# include <stdlib.h>
# else
extern char *getenv ( );
# endif

# define MaxEntries 2048
# define MaxPathLen 2048

# define Waiting   0
# define Canceled -1
# define Okayed    1

struct file_dialog {
    Widget	   shell;		/* transientShell  <specified>	  */
    Widget	   layout;		/*	Layout  layout		  */
    Widget	   label;		/*	     Label  label	  */
    Widget	   directory;		/*	     Label  directory	  */
    Widget	   entry;		/*	     AsciiText  entry	  */
    Widget	   viewport;		/*	     Viewport  viewport	  */
    Widget	   list;		/*		  List  list	  */
    Widget         toggle1;		/*	     Toggle  toggle1      */
    Widget         toggle2;		/*	     Toggle  toggle2      */
    Widget         toggle1_name;	/*	     Toggle  toggle1_name */
    Widget         toggle2_name;	/*	     Toggle  toggle2_name */
    Widget	   okay;		/*	     Command  okay	  */
    Widget	   cancel;		/*	     Command  cancel	  */
    String	   path;
    String	   displayed;
    XtCallbackProc callback;
    XtPointer	   client_data;
    int		   status;
};


/* Resources */

static Pixel highlight;

static String dummy_list [ ] = {
    "                              ", "", "", "", "", "", "", "", "", "", NULL
};

static char layout_string [ ] =
"vertical { 4 \
 horizontal { 4 label <+inf -100% *> 4 } 4 \
 horizontal { 4 directory <+inf -100% *> 4 } 4 \
 horizontal { 4 entry <+inf -100% *> 4 } 4 \
 horizontal { 4 viewport <+inf -100% * +inf -100%> 4 } 4 \
 horizontal { 4 toggle1 2 toggle1_name 4 <+inf -100%> \
                toggle2 2 toggle2_name 4 } 4 \
 horizontal { 4 okay 4 <+inf -100%> cancel 4 } 4 }";

static Arg color_args [ ] = {
    {XtNborderColor, (XtArgVal) &highlight},
};

static Arg layout_args [ ] = {
    {XtNlayout, (XtArgVal) NULL},
};

static Arg label_args [ ] = {
    {XtNresize,      (XtArgVal) True},
    {XtNjustify,     (XtArgVal) XtJustifyLeft},
    {XtNborderWidth, (XtArgVal) 0},
};

static Arg directory_args [ ] = {
    {XtNresize,      (XtArgVal) True},
    {XtNjustify,     (XtArgVal) XtJustifyLeft},
    {XtNborderWidth, (XtArgVal) 0},
};

static Arg entry_args [ ] = {
    {XtNresize,      (XtArgVal) XawtextResizeWidth},
    {XtNeditType,    (XtArgVal) XawtextEdit},
    {XtNborderWidth, (XtArgVal) 0},
};

static Arg viewport_args [ ] = {
    {XtNallowVert,   (XtArgVal) True},
    {XtNuseRight,    (XtArgVal) True},
    {XtNforceBars,   (XtArgVal) True},
    {XtNborderWidth, (XtArgVal) 0},
};

static Arg list_args [ ] = {
    {XtNdefaultColumns, (XtArgVal) 1},
    {XtNforceColumns,   (XtArgVal) 1},
    {XtNresize,         (XtArgVal) True},
    {XtNlist,           (XtArgVal) dummy_list},
};

static Arg name_args [ ] = {
    {XtNlabel,       (XtArgVal) ""},
    {XtNborderWidth, (XtArgVal) 0},
};

static Arg toggle_args [ ] = {
    {XtNlabel,	     (XtArgVal) " "},
};

/* Translation tables */

static String entry_table =
"<Key>Return:   FileDialogOkay()\n\
 <Key>Escape:   FileDialogCancel()\n\
 <Btn1Down>:    SetFocus() select-start()";

static XtTranslations entry_translations;

static String viewport_table =
"<Key>Return:   FileDialogOkay()\n\
 <Key>Escape:   FileDialogCancel()\n\
 <Btn1Down>:    SetFocus()";

static XtTranslations viewport_translations;

static String list_table =
"<Btn1Down>(2): Set() Notify() FileDialogOkay()\n\
 <Btn1Down>:    Set() Notify()\n\
 <Btn2Down>:    Set() Notify() FileDialogOkay()";

static XtTranslations list_translations;

static String okay_table =
"<Key>Return:   AutoRepeat(off) set()\n\
 <KeyUp>Return: AutoRepeat(saved) notify() unset()\n\
 <Key>space:    AutoRepeat(off) set()\n\
 <KeyUp>space:  AutoRepeat(saved) notify() unset()\n\
 <Key>Escape:   FileDialogCancel()";

static XtTranslations okay_translations;

static String cancel_table =
"<Key>Return:   AutoRepeat(off) set()\n\
 <KeyUp>Return: AutoRepeat(saved) notify() unset()\n\
 <Key>space:    AutoRepeat(off) set()\n\
 <KeyUp>space:  AutoRepeat(saved) notify() unset()\n\
 <Key>Escape:   FileDialogCancel()";

static XtTranslations cancel_translations;

static String toggle_table = 
"<Key>Return: FileDialogOkay()\n\
 <Key>Escape: FileDialogCancel()\n\
 <Key>space:  toggle() notify()";

static XtTranslations toggle_translations;


/************************************************************************
 * Function:	DisplayPath						*
 *									*
 * Description:	Displays the path of the current directory by possibly	*
 *		shortening the directory at the front so that it fits.	*
 ************************************************************************/

static void DisplayPath (FileDialog filed, Dimension width)
{
    Arg		 args [1];
    int		 length;
    String	 ptr;
    XFontStruct	*font;


    /* Retrieve the width if necessary. */

    if (width == 0) {
	XtSetArg (args [0], XtNwidth, &width);
	XtGetValues (filed -> directory, args, 1);
    }


    /* Retrieve the font. */

    XtSetArg (args [0], XtNfont, &font);
    XtGetValues (filed -> directory, args, 1);


    /* Prepare to shorten string so that it will fit. */

    length = strlen (filed -> path);
    ptr = filed -> path;

    width -= GetTextWidth (font, "...", 3) + 4;


    /* Keep shortening the string until it fits. */

    while (GetTextWidth (font, ptr, length) > width && length > 1) {
	ptr ++;
	length --;
    }


    /* Display the possibly shortened string. */

    if (ptr != filed -> path)
	sprintf (filed -> displayed, "...%s", ptr);
    else
	strcpy (filed -> displayed, filed -> path);

    XtSetArg (args [0], XtNlabel, filed -> displayed);
    XtSetValues (filed -> directory, args, 1);
}


/************************************************************************
 * Function:	ResizeHandler						*
 *									*
 * Description:	Event handler called upon StructureNotify events.  If	*
 *		the event is specifically a ConfigureNotify event then	*
 *		the path to the current directory is redisplayed within	*
 *		the new window size.  (We subtract 8 to account for the	*
 *		4 pixel space between the label width and the edge of	*
 *		the shell window; we use an event handler rather than a	*
 *		translation so that we can pass the file dialog as	*
 *		client data.						*
 ************************************************************************/

static void ResizeHandler (Widget w, XtPointer client_data, XEvent *event, Boolean *cont)
{
    if (event -> type == ConfigureNotify)
	DisplayPath ((FileDialog) client_data, event -> xconfigure.width - 8);
}


/************************************************************************
 * Function:	SortEntries						*
 *									*
 * Description:	Used by qsort() to compare two strings; directories	*
 *		(which end with a /) are always placed before files	*
 ************************************************************************/

static int SortEntries (String *s1, String *s2)
{
    String dir1;
    String dir2;


    dir1 = strchr (*s1, '/');
    dir2 = strchr (*s2, '/');

    if (dir1 != NULL && dir2 == NULL)
	return -1;

    if (dir1 == NULL && dir2 != NULL)
	return 1;

    return strcmp (*s1, *s2);
}


/************************************************************************
 * Function:	ReadDirectory						*
 *									*
 * Description:	Reads the entries of the specified directory, sorts the	*
 *		entries, and updates the associated list widget with	*
 *		the entries.  Directories are placed before files in	*
 *		sorted order and directory names end with a /.  If the	*
 *		directory cannot be opened, False is returned.		*
 *		Otherwise, True is returned.				*
 ************************************************************************/

static Boolean ReadDirectory (FileDialog filed, String name)
{
    int		   count;
    DIR		  *dirp;
    struct stat	   buf;
    struct dirent *entry;
    static String  list [MaxEntries] = {NULL};
    char	   buffer [MaxPathLen];


    if ((dirp = opendir (name)) == NULL)
	return False;

    count = 0;
    while ((entry = readdir (dirp)) != NULL && count < MaxEntries) {
	if (!strcmp (entry -> d_name, "."))
	    continue;

	XtFree (list [count]);
	sprintf (buffer, "%s/%s", name, entry -> d_name);
	if (!stat (buffer, &buf) && S_ISDIR (buf.st_mode)) {
	    sprintf (buffer, "%s/", entry -> d_name);
	    list [count ++] = XtNewString (buffer);
	} else
	    list [count ++] = XtNewString (entry -> d_name);
    }

    closedir (dirp);
    qsort (list, count, sizeof (String), SortEntries);

    XawListChange (filed -> list, list, count, 0, True);
    XawViewportSetCoordinates (filed -> viewport, 0, 0);
    return True;
}


/************************************************************************
 * Function:	CopySelected						*
 *									*
 * Description:	A callback for the List widget which copies the current	*
 *		entry to the entry widget.				*
 ************************************************************************/

static void CopySelected (Widget w, XtPointer client_data, XtPointer call_data)
{
    FileDialog		 filed;
    XawListReturnStruct *info;


    filed = (FileDialog) client_data;
    info = (XawListReturnStruct *) call_data;
    SetTextString (filed -> entry, info -> string);
}


/************************************************************************
 * Function:	Okay							*
 *									*
 * Description:	A callback for the okay button which forms a path name	*
 *		from the directory and current entry.  If the entry	*
 *		is an absolute path (begins with / or ~) then the	*
 *		current directory is ignored.  The path name is then	*
 *		normalized (removing .., //, and ~).  If the path names	*
 *		a file then that file is selected.  Otherwise, the path	*
 *		is used as a directory which is then read.		*
 ************************************************************************/

static void Okay (Widget w, XtPointer client_data, XtPointer call_data)
{
    char	   old;
    char	   buffer [MaxPathLen];
    Arg		   args   [1];
    String	   dest;
    String	   string;
    String	   ptr;
    FileDialog	   filed;
    struct passwd *entry;


    filed = (FileDialog) client_data;

    XtSetArg (args [0], XtNstring, &string);
    XtGetValues (filed -> entry, args, 1);


    /* If the entry is absolute ignore the current directory.  Otherwise,
       concatenate the entry and directory to form the new path. */

    if (string [0] == '/' || string [0] == '~')
	strcpy (buffer, string);
    else {
	strcpy (buffer, filed -> path);
	strcat (buffer, string);
    }


    /* Expand the tilde. */

    ptr = buffer;
    dest = filed -> path;

    if (!strncmp (buffer, "~/", 2) || !strcmp (buffer, "~")) {
	strcpy (dest, getenv ("HOME"));
	dest += strlen (dest);
	ptr ++;

    } else if (buffer [0] == '~') {
	while (*ptr && *ptr != '/')
	    ptr ++;

	old = *ptr;
	*ptr = 0;

	if ((entry = getpwnam (buffer + 1)) != NULL) {
	    strcpy (dest, entry -> pw_dir);
	    dest += strlen (dest);
	    *ptr = old;
	} else
	    ptr = buffer;
    }


    /* Finish normalizing the directory entry removing any .. or //. */

    while (*ptr) {
	if (!strncmp (ptr, "//", 2) || !strcmp (ptr, "/"))
	    ptr ++;

	else if (!strncmp (ptr, "/../", 4) || !strcmp (ptr, "/..")) {
	    while (dest != filed -> path && *dest != '/')
		dest --;
	    ptr +=3;

	} else
	    *dest ++ = *ptr ++;

	*dest = 0;
    }

    if (filed -> path [0] == 0)
	strcpy (filed -> path, "/");


    /* Try to read the normalized path as a directory. */

    if (ReadDirectory (filed, filed -> path) == True) {


	/* Indicate the new directory and an empty entry. */

	if (strcmp (filed -> path, "/"))
	    strcat (filed -> path, "/");

	DisplayPath (filed, 0);
        SetTextString (filed -> entry, "");


    /* A file was selected.  If this is the initial suggestion then decompose
       the suggestion into a directory and file name and use them. */

    } else if (call_data != NULL) {
	ptr = strrchr (filed -> path, '/');

        SetTextString (filed -> entry, ptr + 1);

	ptr [1] = 0;
	DisplayPath (filed, 0);
	ReadDirectory (filed, filed -> path);


    /* A file was selected.  Call the callback or set the
       selected flag, whichever is appropriate. */

    } else if (filed -> callback != NULL)
	filed -> callback (filed -> entry, filed -> client_data, filed -> path);

    else
	filed -> status = Okayed;
}

/************************************************************************
 * Function:	CheckToggle						*
 *									*
 * Description:	                     					*
 ************************************************************************/

static void CheckToggle (Widget w, XtPointer client_data, XtPointer call_data)
{
    FileDialog		filed;
    Boolean		state;
    Arg			args [1];

    filed = (FileDialog) client_data;

    XtSetArg (args [0], XtNstate, &state);
    XtGetValues (w, args, 1);

    if (state) {
       XtSetArg (args [0], XtNstate, False);
       if (w == filed -> toggle1)
          XtSetValues (filed -> toggle2, args, 1);
       else
          XtSetValues (filed -> toggle1, args, 1);
    }
    else {
       XtSetArg (args [0], XtNstate, True);
       XtSetValues (w, args, 1);
    }
}

/************************************************************************
 * Function:	Cancel							*
 *									*
 * Description:	A callback for the cancel button which indicates that	*
 *		the file dialog has been canceled by either calling the	*
 *		registered callback with a NULL string or by setting	*
 *		the canceled flag.					*
 ************************************************************************/

static void Cancel (Widget w, XtPointer client_data, XtPointer call_data)
{
    FileDialog filed;


    filed = (FileDialog) client_data;

    if (filed -> callback != NULL)
	filed -> callback (filed -> entry, filed -> client_data, NULL);
    else
	filed -> status = Canceled;
}


/************************************************************************
 * Function:	FileDialogOkay						*
 *									*
 * Description:	An action procedure which emulates pressing of the okay	*
 *		button.							* 
 ************************************************************************/

static void FileDialogOkay (Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    if (XtClass (w) == listWidgetClass)
	w = XtParent (w);

    w = XtNameToWidget (XtParent (w), "okay");
    XtCallCallbacks (w, XtNcallback, NULL);
}


/************************************************************************
 * Function:	FileDialogCancel					*
 *									*
 * Description:	An action procedure which emulates pressing of the	*
 *		cancel button.						*
 ************************************************************************/

static void FileDialogCancel (Widget w, XEvent *event, String *params, Cardinal *num_params)
{
    if (event -> type == ClientMessage)
	w = XtNameToWidget (w, "layout.cancel");
    else
	w = XtNameToWidget (XtParent (w), "cancel");

    XtCallCallbacks (w, XtNcallback, NULL);
}


/************************************************************************
 * Function:	FileDialogCreate					*
 *									*
 * Description:	Creates and returns a new file dialog box.  The dialog	*
 *		can be popped up using FileDialogSelect() which forces	*
 *		the user to make a selection, or by using FileDialog-	*
 *		Popup() which will call a callback when a file is	*
 *		selected or the dialog is canceled.  The dialog can	*
 *		then be popped down using FileDialogPopdown().		*
 ************************************************************************/

FileDialog FileDialogCreate (Widget parent, String name, char **toggle_labels)
{
    Arg			args [1];
    Widget		group [6];
    unsigned		group_count;
    unsigned		mask;
    FileDialog		filed;
    static XtAppContext	app_context = NULL;
    static XtActionsRec actions [ ] =
			{{"FileDialogOkay",   FileDialogOkay},
			 {"FileDialogCancel", FileDialogCancel}};


    /* Perform one time initialization. */

    if (app_context == NULL) {
	app_context = XtWidgetToApplicationContext (parent);
	XtAppAddActions (app_context, actions, XtNumber (actions));
	AddAutoRepeatAction (app_context);

	layout_args [0].value = StringToLayout (parent, layout_string);

	entry_translations    = XtParseTranslationTable (entry_table);
	viewport_translations = XtParseTranslationTable (viewport_table);
	list_translations     = XtParseTranslationTable (list_table);
	okay_translations     = XtParseTranslationTable (okay_table);
	cancel_translations   = XtParseTranslationTable (cancel_table);
        toggle_translations   = XtParseTranslationTable (toggle_table);
    }


    /* Create the file dialog and its widgets. */

    filed = XtNew (struct file_dialog);

    filed -> path      = (String) XtMalloc (MaxPathLen);

    filed -> displayed = (String) XtMalloc (MaxPathLen);

    filed -> shell     = XtCreatePopupShell (name,
			 transientShellWidgetClass, parent,
			 NULL, 0);

    filed -> layout    = XtCreateManagedWidget ("layout",
			 layoutWidgetClass, filed -> shell,
			 layout_args, XtNumber (layout_args));

    filed -> label     = XtCreateManagedWidget ("label",
			 labelWidgetClass, filed -> layout,
			 label_args, XtNumber (label_args));

    filed -> directory = XtCreateManagedWidget ("directory",
			 labelWidgetClass, filed -> layout,
			 directory_args, XtNumber (directory_args));

    filed -> entry     = XtCreateManagedWidget ("entry",
			 asciiTextWidgetClass, filed -> layout,
			 entry_args, XtNumber (entry_args));

    filed -> viewport  = XtCreateManagedWidget ("viewport",
			 viewportWidgetClass, filed -> layout, 
			 viewport_args, XtNumber (viewport_args));

    filed -> list      = XtCreateManagedWidget ("list",
			 listWidgetClass, filed -> viewport,
			 list_args, XtNumber (list_args));
 
    filed -> okay      = XtCreateManagedWidget ("okay",
			 commandWidgetClass, filed -> layout,
			 NULL, 0);

    filed -> cancel    = XtCreateManagedWidget ("cancel",
			 commandWidgetClass, filed -> layout,
			 NULL, 0);

    if (toggle_labels != NULL) {
       filed -> toggle1   = XtCreateManagedWidget ("toggle1",
                            toggleWidgetClass, filed -> layout,
                            toggle_args, XtNumber (toggle_args));

       name_args [0].value = (XtArgVal) toggle_labels [0];
       filed -> toggle1_name = XtCreateManagedWidget ("toggle1_name", 
                               labelWidgetClass, filed -> layout, name_args, 
                               XtNumber (name_args));

       filed -> toggle2   = XtCreateManagedWidget ("toggle2",
                            toggleWidgetClass, filed -> layout,
                            toggle_args, XtNumber (toggle_args));

       name_args [0].value = (XtArgVal) toggle_labels [1];
       filed -> toggle2_name = XtCreateManagedWidget ("toggle2_name", 
                               labelWidgetClass, filed -> layout, name_args, 
                               XtNumber (name_args));
    }
    else {
       filed -> toggle1 = NULL;
       filed -> toggle2 = NULL;
    } 


    /* Create a tab group for the file dialog. */

    group_count = 0;

    group [group_count++] = filed -> entry;
    group [group_count++] = filed -> viewport;
    if (toggle_labels != NULL) {
       group [group_count++] = filed -> toggle1;
       group [group_count++] = filed -> toggle2;
    }
    group [group_count++] = filed -> okay;
    group [group_count++] = filed -> cancel;

    XtGetValues (filed -> layout, color_args, XtNumber (color_args));
    CreateTabGroup (filed -> shell, group, group_count, highlight, True);
    XtRealizeWidget (filed -> shell);


    /* Add the translations to each widget. */

    AddDeleteWindowProtocol   (filed -> shell, "FileDialogCancel()");
    ListAddCursorTranslations (filed -> viewport);
    ListAddCursorAccelerators (filed -> viewport, filed -> entry);
    AddScrollableTextTranslations (filed -> entry);

    XtOverrideTranslations (filed -> entry,    entry_translations);
    XtOverrideTranslations (filed -> viewport, viewport_translations);
    XtOverrideTranslations (filed -> okay,     okay_translations);
    XtOverrideTranslations (filed -> cancel,   cancel_translations);

    XtSetArg (args [0], XtNtranslations, list_translations);
    XtSetValues (filed -> list, args, 1);

    mask = StructureNotifyMask;
    XtAddEventHandler (filed -> directory, mask, False, ResizeHandler, filed);



    /* Add the necessary callbacks. */

    XtAddCallback (filed -> list, XtNcallback, CopySelected, (XtPointer) filed);
    XtAddCallback (filed -> cancel, XtNcallback, Cancel, (XtPointer) filed);
    XtAddCallback (filed -> okay, XtNcallback, Okay, (XtPointer) filed);
  
    if (toggle_labels != NULL) {
       XtAddCallback (filed -> toggle1, XtNcallback, 
                      CheckToggle, (XtPointer) filed);
       XtAddCallback (filed -> toggle2, XtNcallback, 
                      CheckToggle, (XtPointer) filed);

       XtOverrideTranslations (filed -> toggle1,  toggle_translations);
       XtOverrideTranslations (filed -> toggle2,  toggle_translations);

       XtSetArg (args [0], XtNstate, True);
       XtSetValues (filed -> toggle1, args, 1);
       XtSetArg (args [0], XtNstate, False);
       XtSetValues (filed -> toggle2, args, 1);
    }

    return filed;
}


/************************************************************************
 * Function:	FileDialogSelect					*
 *									*
 * Description:	Pops up the file dialog with the specified shell title,	*
 *		label, and suggestion and waits until a selection is	*
 *		made or the dialog is canceled; answer will point to	*
 *		the selection or NULL if the dialog was canceled	*
 ************************************************************************/

void FileDialogSelect (FileDialog filed, String title, String label, String suggestion, String *answer, String *toggle)
{
    XEvent       event;
    XtAppContext app_context;


    FileDialogPopup (filed, title, label, suggestion, NULL, NULL);

    filed -> status = Waiting;
    app_context = XtWidgetToApplicationContext (filed -> shell);

    while (filed -> status == Waiting) {
	XtAppNextEvent (app_context, &event);
	XtDispatchEvent (&event);
    }

    FileDialogPopdown (filed);

    if (answer != NULL)
	*answer = filed -> status == Canceled ? NULL : filed -> path;

    if (toggle != NULL && filed -> toggle1 != NULL)
       *toggle = FileDialogToggle (filed);
}

/************************************************************************
 * Function:	FileDialogToggle					*
 *									*
 * Description:	Returns the name of the currently active toggle button.	*
 ************************************************************************/

String FileDialogToggle (FileDialog filed)
{
    Arg     args [1];
    Boolean state;
    String  label;


    XtSetArg (args [0], XtNstate, &state); 
    XtGetValues (filed -> toggle1, args, 1);

    XtSetArg (args [0], XtNlabel, &label);

    if (state) 
       XtGetValues (filed -> toggle1_name, args, 1);
    else 
       XtGetValues (filed -> toggle2_name, args, 1);

    return label;
}

/************************************************************************
 * Function:	FileDialogSetToggles					*
 *									*
 * Description:	Sets the labels for the toggle buttons of the specified	*
 *		file dialog.						*
 ************************************************************************/

void FileDialogSetToggles (FileDialog filed, String label1, String label2)
{
    Arg args [1];


    if (filed -> toggle1 != NULL && filed -> toggle2 != NULL) {
	XtSetArg (args [0], XtNlabel, label1);
	XtSetValues (filed -> toggle1_name, args, 1);

	XtSetArg (args [0], XtNlabel, label2);
	XtSetValues (filed -> toggle2_name, args, 1);
    }
}

/************************************************************************
 * Function:	FileDialogPopup						*
 *									*
 * Description: Pops up the file dialog with the specified shell title,	*
 *		label, and suggestion.  If the suggestion is NULL then	*
 *		the current working directory is used.  If the callback	*
 *		is not NULL then it will be called upon a selection or	*
 *		cancelation with the specified client data.		*
 ************************************************************************/

void FileDialogPopup (FileDialog filed, String title, String label, String suggestion, XtCallbackProc callback, XtPointer client_data)
{
    Arg args [1];


    XtSetArg (args [0], XtNtitle, title);
    XtSetValues (filed -> shell, args, 1);

    XtSetArg (args [0], XtNlabel, label);
    XtSetValues (filed -> label, args, 1);

    if (suggestion == NULL)
	getcwd (filed -> path, MaxPathLen);
    else if (suggestion [0] == '/' || suggestion [0] == '~')
	strcpy (filed -> path, suggestion);
    else {
	getcwd (filed -> path, MaxPathLen);
	strcat (filed -> path, "/");
	strcat (filed -> path, suggestion);
    }

    SetTextString (filed -> entry, filed -> path);

    filed -> callback = callback;
    filed -> client_data = client_data;

    SetFocus (filed -> entry);
    XtCallCallbacks (filed -> okay, XtNcallback, (XtPointer) filed);
    XtPopup (filed -> shell, callback == NULL ? XtGrabExclusive : XtGrabNone);
}


/************************************************************************
 * Function:	FileDialogPopdown					*
 *									*
 * Descripion:	Pops down the specified file dialog			*
 ************************************************************************/

void FileDialogPopdown (FileDialog filed)
{
    XtPopdown (filed -> shell);
}
