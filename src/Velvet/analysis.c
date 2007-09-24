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
 * File:	analysis.c						*
 *									*
 * Description:	This file contains the private and public function and	*
 *		type definitions for the analysis parameters dialog	*
 *		box.							*
 ************************************************************************/

# include <stdio.h>
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Shell.h>
# include <X11/Xaw/AsciiText.h>
# include <X11/Xaw/Command.h>
# include <X11/Xaw/Toggle.h>
# include <X11/Xaw/Label.h>
# include "Layout.h"
# include "TabGroup.h"
# include "Analysis.h"
# include "Solution.h"
# include "util.h"
# include "problem.h"
# include "allocate.h"
# include "procedures.h"
# include "error.h"

# ifndef X_NOT_STDC_ENV
# include <stdlib.h>
# else
extern int    atoi ( );
# endif

# ifndef XtNshadowWidth
# define XtNshadowWidth "shadowWidth"
# endif

static unsigned		base;
static unsigned		Button = 1;
static unsigned		Forced = 2;

struct analysis_dialog {
    Widget  shell;		/* topLevelShell  <specified>	      */
    Widget  layout;		/*	Layout  layout	     	      */
    Widget  title;		/*	     AsciiText title	      */
    Widget  static_bt;		/*	     Toggle static_bt	      */
    Widget  transient;		/*           Toggle transient	      */
    Widget  spectral;		/*	     Toggle spectral          */
    Widget  modal;		/*	     Toggle modal	      */
    Widget  static_thermal;	/*	     Toggle static_thermal    */ 
    Widget  transient_thermal;	/*	     Toggle transient_thermal */
    Widget  static_incremental; /*	     Toggle static_incremental */
    Widget  static_substitution;/*           Toggle static_substitution */
    Widget  Rk;			/*           AsciiText Rk  	      */
    Widget  Rm;			/*           AsciiText Rm  	      */
    Widget  step;		/*           AsciiText step	      */
    Widget  stop;		/*           AsciiText stop           */
    Widget  start;		/*           AsciiText start          */
    Widget  beta;		/*	     AsciiText beta	      */
    Widget  gamma;		/*           AsciiText gamma	      */
    Widget  alpha;		/*           AsciiText alpha          */
    Widget  iterations;		/*	     AsciiText iterations     */
    Widget  relaxation;		/*	     AsciiText relaxation     */
    Widget  tolerance;		/*           AsciiText tolerance      */
    Widget  load_steps;		/*	     AsciiText load_steps     */
    Widget  input_node;		/*           AsciiText input_node     */
    Widget  input_dof [6];	/*           Toggle input_dof [1-6]   */
    Widget  dof [6];		/*	     Toggle dof [1-6]	      */
    Widget  consistent;		/*           Toggle consistent        */
    Widget  lumped;		/*           Toggle lumped            */ 
    Widget  left;		/*     	     Command left	      */
    Widget  right;		/*	     Command right	      */
    Widget  node [6];		/*	     AsciiText node[1-6]      */
    Widget  help;               /*           MenuButton  help         */
    Widget  accept;             /*           Command  accept          */
    Widget  solve;		/*           Command  solve   	      */
    Widget  animate;		/*           Command  animate         */
    Widget  dismiss;            /*           Command  dismiss         */
};


static String labels [ ] = {
    "title:", 
    "static", "static-thermal", "spectral",
    "transient", "transient-thermal", "modal", 
    "static-incremental", "static-substitution",
    "output DOF:","input DOF:",
    "Tx", "Ty", "Tz", "Rx", "Ry", "Rz",
    "Tx", "Ty", "Tz", "Rx", "Ry", "Rz",
    "consistent mass matrix", "lumped mass matrix", 
    "output nodes:"
};

static String label_names [ ] = {
    "title_name", 
    "static_name", "static_thermal_name", "spectral_name",
    "transient_name", "transient_thermal_name", "modal_name",
    "static_incremental_name", "static_substitution_name",
    "dofs_name", "input_dof_name",
    "tx_name", "ty_name", "tz_name", "rx_name", "ry_name", "rz_name",
    "in_tx_name", "in_ty_name", "in_tz_name", 
    "in_rx_name", "in_ry_name", "in_rz_name",
    "consistent_name","lumped_name",
    "nodes_name"
};

static String parameter_labels [ ] = {
    "start:", "stop:", "step:", 
    "b:", "g:", "a:", 
    "Rk:", "Rm:", "relaxation:",
    "tolerance:", "load-steps:", 
    "iterations:",
    "input node:"
};

static String parameter_names [ ] = {
    "start_name", "stop_name", "step_name", 
    "beta_name", "gamma_name", "alpha_name",
    "Rk_name", "Rm_name", "relaxation_name",
    "tolerance_name", "load_steps_name",
    "iterations_name",
    "input_node_name"
};

static String parameter_help [ ] = {
    "starting frequency (=0.0 for time domain) or starting load range value",
    "total time duration, final frequency, or final load range value",
    "time or frequency increment in numerical integration or load range increment",
    "Newmark parameter, =0.25 for trapezoidal",
    "Newmark parameter, =0.5 for trapezoidal",
    "HHT parameter, =0 for standard Newmark",
    "global Rayleigh stiffness damping",
    "global Rayleigh mass damping",
    "over- or under- iterative update relaxation factor", 
    "convergence tolerance",
    "number of load steps to use in non-linear analysis",
    "maximum number of iterations",
    "input node number for load range analysis",
};

/* Resources */

static Pixel highlight;

static char layout_string [ ] =
"vertical { \
    4 \
    horizontal { \
       4 \
       title_name \
       2 \
       title <+inf -100% *> \
       4 \
    } \
    4 \
    separator5 <+inf -100% *> \
    4 \
    horizontal { \
       4 \
       vertical { \
          horizontal { \
             static 2 static_name \
          } \
          horizontal { \
             static_incremental 2 static_incremental_name \
          } \
       } \
       8 <+inf -100%> \
       vertical { \
          horizontal { \
             static_substitution 2 static_substitution_name \
          } \
          horizontal { \
             static_thermal 2 static_thermal_name \
          } \
       } \
       8 <+inf -100%> \
       vertical { \
          horizontal { \
             transient 2 transient_name \
          } \
          horizontal { \
             transient_thermal 2 transient_thermal_name \
          } \
       } \
       8 <+inf -100%> \
       vertical { \
          horizontal { \
             spectral 2 spectral_name \
          } \
          horizontal { \
             modal 2 modal_name \
          } \
       } \
       4 \
    } \
    4 \
    separator2 <+inf -100% *> \
    4 \
    horizontal { \
       4 \
       consistent \
       4 \
       consistent_name \
       16 \
       lumped \
       4 \
       lumped_name \
       4 \
    } \
    4 \
    separator6 <+inf -100% *> \
    4 \
    horizontal { \
       vertical { \
          ((height start - height start_name) / 2) \
          start_name \
          ((height start - height start_name) / 2) \
          4 \
          ((height beta - height beta_name) / 2) \
          beta_name \
          ((height beta - height beta_name) / 2) \
          4 \
          ((height iterations - height iterations_name) / 2) \
          iterations_name \
          ((height iterations - height iterations_name) / 2) \
       } \
       1 \
       vertical { \
          start \
          4 \
          beta \
          4 \
          iterations \
       } \
       8 \
       vertical { \
          ((height stop - height stop_name) / 2) \
          stop_name \
          ((height stop - height stop_name) / 2) \
          4 \
          ((height gamma - height gamma_name) / 2) \
          gamma_name \
          ((height gamma - height gamma_name) / 2) \
          4 \
          ((height tolerance - height tolerance_name) / 2) \
          tolerance_name \
          ((height tolerance - height tolerance_name) / 2) \
       } \
       1 \
       vertical { \
          stop \
          4 \
          gamma \
          4 \
          tolerance \
       } \
       8 \
       vertical { \
          ((height step - height step_name) / 2) \
          step_name \
          ((height step - height step_name) / 2) \
          4 \
          ((height alpha - height alpha_name) / 2) \
          alpha_name \
          ((height alpha - height alpha_name) / 2) \
          4 \
          ((height load_steps - height load_steps_name) / 2) \
          load_steps_name \
          ((height load_steps - height load_steps_name) / 2) \
       } \
       1 \
       vertical { \
          step \
          4 \
          alpha \
          4 \
          load_steps \
       } \
       8 \
       vertical { \
          ((height Rk - height Rk_name) / 2) \
          Rk_name \
          ((height Rk - height Rk_name) / 2) \
          4 \
          ((height Rm - height Rm_name) / 2) \
          Rm_name \
          ((height Rm - height Rm_name) / 2) \
          4 \
          ((height relaxation - height relaxation_name) / 2) \
          relaxation_name \
          ((height relaxation - height relaxation_name) / 2) \
       } \
       1 \
       vertical { \
          Rk \
          4 \
          Rm \
          4 \
          relaxation \
       } \
       4 \
    } \
    4 \
    separator1 <+inf -100% *> \
    4 \
    horizontal { \
       4 \
       input_node_name \
       2 \
       input_node \
       20 \
       input_dof_name \
       10 \
       in_tx_name 2 input_dof1 \
       6 \
       in_ty_name 2 input_dof2 \
       6 \
       in_tz_name 2 input_dof3 \
       6 \
       in_rx_name 2 input_dof4 \
       6 \
       in_ry_name 2 input_dof5 \
       6 \
       in_rz_name 2 input_dof6 \
       4 \
    } \
    4 \
    separator3 <+inf -100% *> \
    4 \
    horizontal { \
       4 \
       nodes_name \
       8 \
       left \
       4 \
       node1 \
       4 \
       node2 \
       4 \
       node3 \
       4 \
       node4 \
       4 \
       node5 \
       4 \
       node6 \
       4 \
       right \
       4 \
    } \
    4 \
    horizontal { \
       4 \
       dofs_name \
       10 \
       tx_name 2 dof1 \
       6 \
       ty_name 2 dof2 \
       6 \
       tz_name 2 dof3 \
       6 \
       rx_name 2 dof4 \
       6 \
       ry_name 2 dof5 \
       6 \
       rz_name 2 dof6 \
       4 \
    } \
    4 \
    separator4 <+inf -100% *> \
    4 \
    horizontal { \
       4 \
       help \
       4 <+inf -100%> \
       solve \
       4 <+inf -100%> \
       animate \
       4 <+inf -100%> \
       accept \
       4 <+inf -100%> \
       dismiss \
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

static Arg text_args [ ] = {
    {XtNeditType,    (XtArgVal) XawtextEdit},
    {XtNborderWidth, (XtArgVal) 0},
    {XtNpieceSize,   (XtArgVal) 32},
    {XtNcursorName,  (XtArgVal) "left_ptr"},
    {XtNwidth,       (XtArgVal) 70},
};

static Arg node_args [ ] = {
    {XtNeditType,    (XtArgVal) XawtextEdit},
    {XtNborderWidth, (XtArgVal) 0},
    {XtNpieceSize,   (XtArgVal) 24},
    {XtNcursorName,  (XtArgVal) "left_ptr"},
    {XtNwidth,       (XtArgVal) 40},
};

/*
static Arg freq_args [ ] = {
    {XtNeditType,    (XtArgVal) XawtextEdit},
    {XtNborderWidth, (XtArgVal) 0},
    {XtNpieceSize,   (XtArgVal) 32},
    {XtNcursorName,  (XtArgVal) "left_ptr"},
    {XtNwidth,       (XtArgVal) 50},
};
*/

static Arg parameter_args [ ] = {
    {XtNlabel,              (XtArgVal) ""},
    {XtNborderWidth,        (XtArgVal) 0},
    {XtNhighlightThickness, (XtArgVal) 0},
    {XtNshadowWidth,        (XtArgVal) 0},
};

static Arg bitmap_args [ ] = {
    {XtNbitmap, (XtArgVal) NULL},
};

static Arg label_args [ ] = {
    {XtNlabel,	            (XtArgVal) ""},
    {XtNborderWidth,        (XtArgVal) 0},
};

static Arg core_args [ ] = {
    {XtNwidth,	(XtArgVal) 3},
    {XtNheight, (XtArgVal) 3},
};

static Arg toggle_args [ ] = {
    {XtNlabel,		    (XtArgVal) " "},
};

#define left_width 12
#define left_height 12
static char left_bits[] = {
   0x00, 0x00, 0x80, 0x01, 0xc0, 0x01, 0xe0, 0x01, 0xf0, 0x01, 0xf8, 0x01,
   0xf8, 0x01, 0xf0, 0x01, 0xe0, 0x01, 0xc0, 0x01, 0x80, 0x01, 0x00, 0x00};

#define right_width 12
#define right_height 12
static char right_bits[] = {
   0x00, 0x00, 0x18, 0x00, 0x38, 0x00, 0x78, 0x00, 0xf8, 0x00, 0xf8, 0x01,
   0xf8, 0x01, 0xf8, 0x00, 0x78, 0x00, 0x38, 0x00, 0x18, 0x00, 0x00, 0x00};

/* Translation tables */

static String text_table =
"<Key>Return: AnalysisDialogAction(accept)\n\
 <Key>Escape: AnalysisDialogAction(dismiss)\n\
 Ctrl<Key>h:  AnalysisDialogAction(help)\n\
 <Btn1Down>:  SetFocus() select-start()";

static XtTranslations text_translations;

static String command_table =
"<Key>Return:   AutoRepeat(off) set()\n\
 <KeyUp>Return: AutoRepeat(saved) notify() unset()\n\
 <Key>space:    AutoRepeat(off) set()\n\
 <KeyUp>space:  AutoRepeat(saved) notify() unset()\n\
 <Key>Escape:   AnalysisDialogAction(dismiss)";

static XtTranslations command_translations;

static String toggle_table =
"<Key>Return:	AnalysisDialogAction(accept)\n\
 <Key>Escape:	AnalysisDialogAction(dismiss)\n\
 <Key>space:	toggle()\n\
 Ctrl<Key>h:	AnalysisDialogAction(help)";

static XtTranslations toggle_translations;

static String radio_table =
"<Key>Return:	AnalysisDialogAction(accept)\n\
 <Key>Escape:	AnalysisDialogAction(dismiss)\n\
 Ctrl<Key>h:	AnalysisDialogAction(help)\n\
 <Key>space:	ToggleAction()";

static XtTranslations radio_translations;

static String help_table =
"<Key>Return: AnalysisDialogAction(accept)\n\
 <Key>Escape: AnalysisDialogAction(dismiss)\n\
 Ctrl<Key>h:  AnalysisDialogAction(help)\n\
 <Key>space:  PostMenu()";

static XtTranslations help_translations;

/* Help message. */

static String help_message ="\
The controls in this dialog are directly analogous to the problem description \
and analysis parameters sections of a FElt input file.  The problem title \
is set with the title text box.  The toggles under the title define the \
analysis mode for this problem.  The other controls define parameters used  \
in the various numerical algorithms that FElt uses to solve a given type \
of problem.  Use the node entries to define which nodes you would like to \
see results for.  The DOF toggles control the degrees of freedom which will \
be analyzed at each of these nodes.";


/************************************************************************
 * Function:	Action							*
 *									*
 * Description:	An action procedure which emulates pressing of the	*
 *		specified button.					*
 ************************************************************************/

static void Action (w, event, params, num_params)
    Widget    w;
    XEvent   *event;
    String   *params;
    Cardinal *num_params;
{
    if (XtClass (w) == topLevelShellWidgetClass)
	w = XtNameToWidget (w, "layout.dismiss");
    else
	w = XtNameToWidget (XtParent (w), params [0]);

    XtCallCallbacks (w, XtNcallback, NULL);
}

/************************************************************************
 * Function:	SetRadioState						*
 *									*
 * Description: Sets a selected analysis type				*
 ************************************************************************/

static void SetRadioState (w, client_data, call_data)
   Widget	w;
   XtPointer	client_data,
		call_data;
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
 * Function:	SetInputDOF						*
 *									*
 * Description: Sets a selected input DOF				*
 ************************************************************************/

static void SetInputDOF (w, client_data, call_data)
   Widget	w;
   XtPointer	client_data,
		call_data;
{
   Arg			args [1];
   Boolean		state;
   static Widget	prev_widget = NULL;

   if (prev_widget == NULL) {
      XtSetArg (args[0], XtNstate, True);
      XtSetValues (w, args, 1);

      prev_widget = w;
      return;
   }

   XtSetArg (args[0], XtNstate, &state);
   XtGetValues (w, args, 1);
   
   if (state && prev_widget != w) {
      XtSetArg (args[0], XtNstate, False);
      XtSetValues (prev_widget, args, 1);
   }

   prev_widget = w;
}

/************************************************************************
 * Function:    ToggleAction                                            *
 *                                                                      *
 * Description: Callback to set the radio state.                        *
 ************************************************************************/

static void ToggleAction (w, event, params, num_params)
    Widget    w;
    XEvent   *event;
    String   *params;
    Cardinal *num_params;
{
    SetRadioState (w, &Forced, NULL);
}

/************************************************************************
 * Function:	GetAnalysisType						*
 *									*
 * Description: Returns the currently selected problem analysis type	*				*
 ************************************************************************/

static AnalysisType GetAnalysisType (analysisd)
   AnalysisDialog	analysisd;
{
   Boolean      state;
   Arg          args[1];
   
   XtSetArg (args [0], XtNstate, &state);

   XtGetValues (analysisd -> static_bt, args, 1);
   if (state) return Static;
   XtGetValues (analysisd -> transient, args, 1);
   if (state) return Transient;
   XtGetValues (analysisd -> spectral, args, 1);
   if (state) return Spectral;
   XtGetValues (analysisd -> modal, args, 1);
   if (state) return Modal;
   XtGetValues (analysisd -> static_thermal, args, 1);
   if (state) return StaticThermal;
   XtGetValues (analysisd -> transient_thermal, args, 1);
   if (state) return TransientThermal;
   XtGetValues (analysisd -> static_substitution, args, 1);
   if (state) return StaticSubstitution;
   XtGetValues (analysisd -> static_incremental, args, 1);
   if (state) return StaticIncremental;

   return 0;
}

/************************************************************************
 * Function:	Accept							*
 *									*
 * Description:								*
 ************************************************************************/

static void Accept (w, client_data, call_data)
    Widget    w;
    XtPointer client_data;
    XtPointer call_data;
{
    Arg		   args [1];
    AnalysisDialog analysisd;
    String	   value;
    Boolean	   state;
    unsigned	   i;
    struct node	   n;

    analysisd = (AnalysisDialog) client_data;

    XtSetArg (args [0], XtNstring,  &value);

    XtGetValues (analysisd -> title, args, 1);
    solution -> title = XtNewString (value);
    problem.title = solution -> title;

    XtGetValues (analysisd -> Rk, args, 1);
    analysis.Rk = exptod (value, NULL);
    XtGetValues (analysisd -> Rm, args, 1);
    analysis.Rm = exptod (value, NULL);
    XtGetValues (analysisd -> start, args, 1);
    analysis.start = exptod (value, NULL);
    XtGetValues (analysisd -> step, args, 1);
    analysis.step = exptod (value, NULL);
    XtGetValues (analysisd -> stop, args, 1);
    analysis.stop = exptod (value, NULL);
    XtGetValues (analysisd -> beta, args, 1);
    analysis.beta = exptod (value, NULL);
    XtGetValues (analysisd -> gamma, args, 1);
    analysis.gamma = exptod (value, NULL);
    XtGetValues (analysisd -> alpha, args, 1);
    analysis.alpha = exptod (value, NULL);
    XtGetValues (analysisd -> tolerance, args, 1);
    analysis.tolerance = exptod (value, NULL);
    XtGetValues (analysisd -> relaxation, args, 1);
    analysis.relaxation = exptod (value, NULL);

    XtGetValues (analysisd -> iterations, args, 1);
    analysis.iterations = atoi (value);
    XtGetValues (analysisd -> load_steps, args, 1);
    analysis.load_steps = atoi (value);

    analysis.input_node = NULL;

    XtGetValues (analysisd -> input_node, args, 1);
    if (strcmp(value, "") != 0) {
       n.number = atoi(value);
       analysis.input_node = TreeSearch (problem.node_tree, &n);

       if (analysis.input_node == NULL) 
          error ("node %d not defined in current problem", n.number);

       SetTextString (analysisd -> input_node, "");
    }

    XtSetArg (args [0], XtNstate, &state);
   
    analysis.input_dof = 0;
    for (i = 0 ; i < 6 ; i++) {
       XtGetValues (analysisd -> input_dof [i], args,1);
       if (state) {
          analysis.input_dof = i + 1;
          break;
       }
    }

    analysis.numdofs = 0;
    for (i = 0 ; i < 6 ; i++) {
       XtGetValues (analysisd -> dof [i], args, 1);
       if (state) 
          analysis.dofs [++ analysis.numdofs] = i + 1;
    }

    XtGetValues (analysisd -> consistent, args, 1);
    if (state)
       analysis.mass_mode = 'c';
    else {
       XtGetValues (analysisd -> lumped, args, 1);
       if (state)
          analysis.mass_mode = 'l';
       else
          analysis.mass_mode = 0;
    }

    problem.mode = GetAnalysisType (analysisd);

    AnalysisDialogUpdate (analysisd, False);
}


/************************************************************************
 * Function:	ShiftNodes						*
 *									*
 * Description:	Shuffle the node entries left and right			*
 ************************************************************************/

static void ShiftNodes (w, client_data, call_data)
    Widget	w;
    XtPointer	client_data;
    XtPointer	call_data;
{
    Arg		    args [1];
    AnalysisDialog  analysisd;
    Node	    current [6];
    Node	    node;
    struct node	    n;
    unsigned	    count;
    String	    value;
    char	    buffer [10];
    unsigned	    i;

    analysisd = (AnalysisDialog) client_data;

   	/*
	 * figure out which entries have numbers in them
	 */

    XtSetArg (args [0], XtNstring,  &value);

    count = 0;
    for (i = 0 ; i < 6 ; i++) {
       XtGetValues (analysisd -> node[i], args, 1);
       n.number = atoi (value);
       if (n.number != 0) {
          node = TreeSearch (problem.node_tree, &n);
          if (node == NULL) {
             error ("node %d is not defined", n.number);
             return;
          }
 
          current [count++] = node;
       }
    }

	/*
	 * if there are new ones, make some room for them
	 */

    if (base + count > analysis.numnodes) {
       analysis.numnodes = base + count;
       ZeroOffset (analysis.nodes);
       analysis.nodes = Reallocate (analysis.nodes, Node, analysis.numnodes);
       UnitOffset (analysis.nodes);
    }
    else if (base + count < analysis.numnodes && base + count > 0)
       analysis.numnodes = base + count;

	/*	
 	 * update the nodes array 
         */

    for (i = 0 ; i < count ; i++) 
       analysis.nodes [base + i + 1] = current [i];
       
	/*
	 * if necessary (and allowed) actually do the shift
	 */

    if (w == analysisd -> right && count > 1) 
       base ++;
    else if (w == analysisd -> left && base > 0) 
       base --;

	/*
	 * update the nodes that are actually displayed
	 */

    for (i = 0 ; i < 6 ; i++) {
       if (base + i < analysis.numnodes)
          sprintf (buffer,"%d", analysis.nodes[base+i+1] -> number);
       else
	  buffer [0] = 0;

       SetTextString (analysisd -> node[i], buffer);
    } 
}

/************************************************************************
 * Function:	CheckMassToggle						*
 *									*
 * Description:	enforces radio behavior on the mass mode toggles	*
 ************************************************************************/

static void CheckMassToggle (w, client_data, call_data)
    Widget	w;
    XtPointer	client_data;
    XtPointer	call_data;
{
    AnalysisDialog	analysisd;
    Boolean		state;
    Arg			args [1];

    analysisd = (AnalysisDialog) client_data;

    XtSetArg (args [0], XtNstate, &state);
    XtGetValues (w, args, 1);

	/*
	 * we enforce radio behavior by only doing anything if the user
	 * is trying to turn something on
	 */

    if (state) {
       XtSetArg (args [0], XtNstate, False); 
       if (w == analysisd -> lumped) 
          XtSetValues (analysisd -> consistent, args, 1);
       else 
          XtSetValues (analysisd -> lumped, args, 1);
    }
    else {
       XtSetArg (args [0], XtNstate, True);
       XtSetValues (w, args, 1);
    }
}

/************************************************************************
 * Function:	Dismiss							*
 *									*
 * Description:	sets the dismiss flag					*
 ************************************************************************/

static void Dismiss (w, client_data, call_data)
    Widget    w;
    XtPointer client_data;
    XtPointer call_data;
{
    AnalysisDialog analysisd;

    analysisd = (AnalysisDialog) client_data;
    XtPopdown (analysisd -> shell);
}

/************************************************************************
 * Function:	ExternalCallback					*
 *									*
 * Description:	calls a function outside the immediate control of 	*
 *		the dialog						*
 ************************************************************************/

static void ExternalCallback (w, client_data, call_data)
    Widget 	w;
    XtPointer	client_data;
    XtPointer	call_data;
{
    AnalysisDialog	analysisd;

    analysisd = (AnalysisDialog) client_data;

    Accept (NULL, client_data, NULL);

    if (w == analysisd -> animate) 
       SetupAnimate ( );
    else if (w == analysisd -> solve)
       SetupAndSolve ( ); 
}

/************************************************************************
 * Function:	AnalysisDialogUpdate					*
 *									*
 * Description:	sets the widgets based on current analysis settings 	*
 ************************************************************************/

# if defined(__STDC__)
void AnalysisDialogUpdate (AnalysisDialog analysisd, Boolean force)
# else
void AnalysisDialogUpdate (analysisd,force)
    AnalysisDialog	analysisd;
    Boolean		force;
# endif
{
    Arg		args [1];
    char	buffer [80];
    unsigned	i;

   SetTextString (analysisd -> title, solution -> title);

	/*
	 * set the analysis mode toggle
	 */

   if (problem.mode == Static || 
       problem.mode == StaticLoadCases ||
       problem.mode == StaticLoadRange) 
      SetRadioState (analysisd -> static_bt, &Forced, NULL); 
   else if (problem.mode == Transient) 
      SetRadioState (analysisd -> transient, &Forced, NULL); 
   else if (problem.mode == Spectral) 
      SetRadioState (analysisd -> spectral, &Forced, NULL); 
   else if (problem.mode == Modal) 
      SetRadioState (analysisd -> modal, &Forced, NULL); 
   else if (problem.mode == StaticThermal) 
      SetRadioState (analysisd -> static_thermal, &Forced, NULL); 
   else if (problem.mode == TransientThermal) 
      SetRadioState (analysisd -> transient_thermal, &Forced, NULL); 
   else if (problem.mode == StaticSubstitution || 
            problem.mode == StaticSubstitutionLoadRange)
      SetRadioState (analysisd -> static_substitution, &Forced, NULL);
   else if (problem.mode == StaticIncremental || 
            problem.mode == StaticIncrementalLoadRange)
      SetRadioState (analysisd -> static_incremental, &Forced, NULL);

	/*
	 * set the text entries
	 */

    sprintf (buffer,"%g",analysis.Rk);
    SetTextString (analysisd -> Rk, buffer);
    sprintf (buffer,"%g",analysis.Rm);
    SetTextString (analysisd -> Rm, buffer);
    sprintf (buffer,"%g",analysis.step);
    SetTextString (analysisd -> step, buffer);
    sprintf (buffer,"%g",analysis.stop);
    SetTextString (analysisd -> stop, buffer);
    sprintf (buffer,"%g",analysis.start);
    SetTextString (analysisd -> start, buffer);
    sprintf (buffer,"%g",analysis.beta);
    SetTextString (analysisd -> beta, buffer);
    sprintf (buffer,"%g",analysis.gamma);
    SetTextString (analysisd -> gamma, buffer);
    sprintf (buffer,"%g",analysis.alpha);
    SetTextString (analysisd -> alpha, buffer);
    sprintf (buffer,"%g",analysis.tolerance);
    SetTextString (analysisd -> tolerance, buffer);
    sprintf (buffer,"%g",analysis.relaxation);
    SetTextString (analysisd -> relaxation, buffer);
    sprintf (buffer,"%d",analysis.iterations);
    SetTextString (analysisd -> iterations, buffer);
    sprintf (buffer,"%d",analysis.load_steps);
    SetTextString (analysisd -> load_steps, buffer);

    if (analysis.input_dof) 
       SetInputDOF (analysisd -> input_dof [analysis.input_dof - 1], NULL, NULL);

    if (analysis.input_node) {
       sprintf (buffer,"%d",analysis.input_node -> number);
       SetTextString (analysisd -> input_node, buffer);
    }

	/*
	 * set the dof toggles
	 */

    XtSetArg (args [0], XtNstate, False);
    for (i = 0 ; i < 6 ; i++)
       XtSetValues (analysisd -> dof [i], args, 1);

    XtSetArg (args [0], XtNstate, True);
    for (i = 1 ; i <= analysis.numdofs ; i++) 
       XtSetValues (analysisd -> dof [analysis.dofs[i] - 1], args, 1);

	/*
	 * set the mass mode toggles
	 */

    XtSetArg (args [0], XtNstate, True);
    if (analysis.mass_mode == 'c') {
       XtSetValues (analysisd -> consistent, args, 1);
       XtSetArg (args [0], XtNstate, False);
       XtSetValues (analysisd -> lumped, args, 1);
    }
    else if (analysis.mass_mode == 'l') {
       XtSetValues (analysisd -> lumped, args, 1);
       XtSetArg (args [0], XtNstate, False);
       XtSetValues (analysisd -> consistent, args, 1);
    }
    else {
       XtSetArg (args [0], XtNstate, False);
       XtSetValues (analysisd -> lumped, args, 1);
       XtSetValues (analysisd -> consistent, args, 1);
    }

	/*
	 * fill in the nodes
	 */

    if (force) {
       for (i = 0 ; i < 6 ; i++) 
          SetTextString (analysisd -> node[i], "");
    }

    ShiftNodes (NULL, (XtPointer) analysisd, NULL);
}

/************************************************************************
 * Function:	AnalysisDialogCreate					*
 *									*
 * Description:	Creates a new analysis dialog.  You would never want to	*
 *		have more than one of these but the interface is kept	*
 *		consistent with those of the other dialogs.		*
 ************************************************************************/

AnalysisDialog AnalysisDialogCreate (parent, name, title)
    Widget parent;
    String name;
    String title;
{
    char		buffer [10];
    Cardinal		i;
    Arg			args [1];
    Widget		group [49];
    AnalysisDialog	analysisd;
    Dimension		width;
    Widget		parameter_help_widget;
    Position		x;
    static Pixmap	left_bitmap;
    static Pixmap	right_bitmap;
    static XtAppContext	app_context = NULL;
    static XtActionsRec	actions [ ] = {{"AnalysisDialogAction", Action},
                                       {"ToggleAction", ToggleAction}};


    /* Perform one time initialization. */

    if (app_context == NULL) {
	app_context = XtWidgetToApplicationContext (parent);
	XtAppAddActions (app_context, actions, XtNumber (actions));
	AddAutoRepeatAction (app_context);

        layout_args [0].value = StringToLayout (parent, layout_string);

        toggle_translations  = XtParseTranslationTable (toggle_table);
        radio_translations   = XtParseTranslationTable (radio_table);
	text_translations    = XtParseTranslationTable (text_table);
	command_translations = XtParseTranslationTable (command_table);
        help_translations    = XtParseTranslationTable (help_table);

        
        left_bitmap = XCreateBitmapFromData (XtDisplay (parent), 
                      RootWindowOfScreen (XtScreen (parent)),
                      left_bits, left_width, left_height);
        right_bitmap = XCreateBitmapFromData (XtDisplay (parent), 
                      RootWindowOfScreen (XtScreen (parent)),
                      right_bits, right_width, right_height);
    }


    /* Create the analysis dialog and its widgets. */

    XtSetArg (shell_args [0], XtNtitle,    title);
    XtSetArg (shell_args [1], XtNiconName, title);

    analysisd = XtNew (struct analysis_dialog);

    analysisd -> shell    = XtCreatePopupShell (name,
			 topLevelShellWidgetClass, parent,
			 shell_args, XtNumber (shell_args));

    analysisd -> layout   = XtCreateManagedWidget ("layout",
			 layoutWidgetClass, analysisd -> shell,
			 layout_args, XtNumber (layout_args));

    analysisd -> title    = XtCreateManagedWidget ("title",
                         asciiTextWidgetClass, analysisd -> layout,
                         text_args, XtNumber (text_args));

    analysisd -> transient = XtCreateManagedWidget ("transient",
                         toggleWidgetClass, analysisd -> layout,
                         toggle_args, XtNumber (toggle_args));

    analysisd -> static_bt = XtCreateManagedWidget ("static",
                         toggleWidgetClass, analysisd -> layout,
                         toggle_args, XtNumber (toggle_args));

    analysisd -> transient_thermal = XtCreateManagedWidget ("transient_thermal",
                         toggleWidgetClass, analysisd -> layout,
                         toggle_args, XtNumber (toggle_args));

    analysisd -> static_thermal = XtCreateManagedWidget ("static_thermal",
                         toggleWidgetClass, analysisd -> layout,
                         toggle_args, XtNumber (toggle_args));

    analysisd -> static_incremental = XtCreateManagedWidget ("static_incremental",
                         toggleWidgetClass, analysisd -> layout,
                         toggle_args, XtNumber (toggle_args));

    analysisd -> static_substitution = XtCreateManagedWidget ("static_substitution",
                         toggleWidgetClass, analysisd -> layout,
                         toggle_args, XtNumber (toggle_args));

    analysisd -> modal     = XtCreateManagedWidget ("modal",
                         toggleWidgetClass, analysisd -> layout,
                         toggle_args, XtNumber (toggle_args));

    analysisd -> spectral = XtCreateManagedWidget ("spectral",
                         toggleWidgetClass, analysisd -> layout,
                         toggle_args, XtNumber (toggle_args));

    analysisd -> Rk       = XtCreateManagedWidget ("Rk",
                         asciiTextWidgetClass, analysisd -> layout,
                         text_args, XtNumber (text_args));

    analysisd -> Rm       = XtCreateManagedWidget ("Rm",
                         asciiTextWidgetClass, analysisd -> layout,
                         text_args, XtNumber (text_args));

    analysisd -> start    = XtCreateManagedWidget ("start",
                         asciiTextWidgetClass, analysisd -> layout,
                         text_args, XtNumber (text_args));

    analysisd -> step	  = XtCreateManagedWidget ("step",
                         asciiTextWidgetClass, analysisd -> layout,
                         text_args, XtNumber (text_args));

    analysisd -> stop     = XtCreateManagedWidget ("stop",
                         asciiTextWidgetClass, analysisd -> layout,
                         text_args, XtNumber (text_args));

    analysisd -> beta	  = XtCreateManagedWidget ("beta",
                         asciiTextWidgetClass, analysisd -> layout,
                         text_args, XtNumber (text_args));

    analysisd -> gamma	  = XtCreateManagedWidget ("gamma",
                         asciiTextWidgetClass, analysisd -> layout,
                         text_args, XtNumber (text_args));

    analysisd -> alpha	  = XtCreateManagedWidget ("alpha",
                         asciiTextWidgetClass, analysisd -> layout,
                         text_args, XtNumber (text_args));

    analysisd -> tolerance  = XtCreateManagedWidget ("tolerance",
                         asciiTextWidgetClass, analysisd -> layout,
                         text_args, XtNumber (text_args));

    analysisd -> load_steps  = XtCreateManagedWidget ("load_steps",
                         asciiTextWidgetClass, analysisd -> layout,
                         text_args, XtNumber (text_args));

    analysisd -> iterations  = XtCreateManagedWidget ("iterations",
                         asciiTextWidgetClass, analysisd -> layout,
                         text_args, XtNumber (text_args));

    analysisd -> relaxation  = XtCreateManagedWidget ("relaxation",
                         asciiTextWidgetClass, analysisd -> layout,
                         text_args, XtNumber (text_args));

    analysisd -> consistent = XtCreateManagedWidget ("consistent",
                         toggleWidgetClass, analysisd -> layout,
                         toggle_args, XtNumber (toggle_args));

    analysisd -> lumped   = XtCreateManagedWidget ("lumped",
                         toggleWidgetClass, analysisd -> layout,
                         toggle_args, XtNumber (toggle_args));

    bitmap_args [0].value = (XtArgVal) left_bitmap; 
    analysisd -> left     = XtCreateManagedWidget ("left",
                          commandWidgetClass, analysisd -> layout,
                          bitmap_args, XtNumber (bitmap_args));
 
    bitmap_args [0].value = (XtArgVal) right_bitmap; 
    analysisd -> right    = XtCreateManagedWidget ("right",
                          commandWidgetClass, analysisd -> layout,
                          bitmap_args, XtNumber (bitmap_args));
 
    analysisd -> accept   = XtCreateManagedWidget ("accept",
			 commandWidgetClass, analysisd -> layout,
			 NULL, 0);

    analysisd -> animate   = XtCreateManagedWidget ("animate",
			 commandWidgetClass, analysisd -> layout,
			 NULL, 0);

    analysisd -> solve   = XtCreateManagedWidget ("solve",
			 commandWidgetClass, analysisd -> layout,
			 NULL, 0);

    analysisd -> dismiss  = XtCreateManagedWidget ("dismiss",
			 commandWidgetClass, analysisd -> layout,
			 NULL, 0);

    for (i = 0 ; i < 6 ; i++) {
       sprintf (buffer, "dof%d", i+1);
       analysisd -> dof[i] = XtCreateManagedWidget (buffer,
                         toggleWidgetClass, analysisd -> layout,
                         toggle_args, XtNumber (toggle_args));

       sprintf (buffer, "input_dof%d", i+1);
       analysisd -> input_dof[i] = XtCreateManagedWidget (buffer,
                         toggleWidgetClass, analysisd -> layout,
                         toggle_args, XtNumber (toggle_args));
    }

    analysisd -> input_node = XtCreateManagedWidget ("input_node",
                      asciiTextWidgetClass, analysisd -> layout,
                      node_args, XtNumber (node_args)); 

    for (i = 0 ; i < 6 ; i++) {
       sprintf (buffer,"node%d", i+1);
       analysisd -> node[i] = XtCreateManagedWidget (buffer,
                         asciiTextWidgetClass, analysisd -> layout,
                         node_args, XtNumber (node_args)); 
    }

    analysisd -> help     = CreateHelpButton (analysisd -> layout, "help");

    for (i = 0 ; i < XtNumber (parameter_labels) ; i++) {
        parameter_args [0].value = (XtArgVal) parameter_labels [i];
        parameter_help_widget = CreateHelpButton (analysisd -> layout, 
                                                  parameter_names [i]);

        XtSetValues (parameter_help_widget, parameter_args, 
                     XtNumber (parameter_args));

        UpdateHelpMessage (parameter_help_widget, parameter_help [i], 200);
    }

    for (i = 0 ; i < XtNumber (labels) ; i++) {
        label_args [0].value = (XtArgVal) labels [i];
        XtCreateManagedWidget (label_names [i], labelWidgetClass,
                   analysisd -> layout, label_args, XtNumber (label_args));
    }

    XtCreateManagedWidget ("separator1", coreWidgetClass, 
               analysisd -> layout, core_args, XtNumber (core_args));
    XtCreateManagedWidget ("separator2", coreWidgetClass, 
               analysisd -> layout, core_args, XtNumber (core_args));
    XtCreateManagedWidget ("separator3", coreWidgetClass, 
               analysisd -> layout, core_args, XtNumber (core_args));
    XtCreateManagedWidget ("separator4", coreWidgetClass, 
               analysisd -> layout, core_args, XtNumber (core_args));
    XtCreateManagedWidget ("separator5", coreWidgetClass, 
               analysisd -> layout, core_args, XtNumber (core_args));
    XtCreateManagedWidget ("separator6", coreWidgetClass, 
               analysisd -> layout, core_args, XtNumber (core_args));


    /* Create a tab group for the analysis dialog. */

    i = 0;
    group [i++] = analysisd -> title;
    group [i++] = analysisd -> static_bt;
    group [i++] = analysisd -> static_incremental;
    group [i++] = analysisd -> static_substitution;
    group [i++] = analysisd -> static_thermal;
    group [i++] = analysisd -> transient;
    group [i++] = analysisd -> transient_thermal;
    group [i++] = analysisd -> spectral;
    group [i++] = analysisd -> modal;

    group [i++] = analysisd -> consistent;
    group [i++] = analysisd -> lumped;
    group [i++] = analysisd -> start; 
    group [i++] = analysisd -> stop; 
    group [i++] = analysisd -> step;
    group [i++] = analysisd -> beta;
    group [i++] = analysisd -> gamma;
    group [i++] = analysisd -> alpha;
    group [i++] = analysisd -> Rk;
    group [i++] = analysisd -> Rm;
    group [i++] = analysisd -> iterations;
    group [i++] = analysisd -> tolerance;
    group [i++] = analysisd -> load_steps;
    group [i++] = analysisd -> relaxation;
  
    group [i++] = analysisd -> input_node;
    group [i++] = analysisd -> input_dof [0];
    group [i++] = analysisd -> input_dof [1];
    group [i++] = analysisd -> input_dof [2];
    group [i++] = analysisd -> input_dof [3];
    group [i++] = analysisd -> input_dof [4];
    group [i++] = analysisd -> input_dof [5];

    group [i++] = analysisd -> dof[0];
    group [i++] = analysisd -> dof[1];
    group [i++] = analysisd -> dof[2];
    group [i++] = analysisd -> dof[3] ;
    group [i++] = analysisd -> dof[4];
    group [i++] = analysisd -> dof[5];
    group [i++] = analysisd -> left;
    group [i++] = analysisd -> right;
    group [i++] = analysisd -> node[0];
    group [i++] = analysisd -> node[1];
    group [i++] = analysisd -> node[2];
    group [i++] = analysisd -> node[3];
    group [i++] = analysisd -> node[4];
    group [i++] = analysisd -> node[5];
    group [i++] = analysisd -> help;
    group [i++] = analysisd -> solve;
    group [i++] = analysisd -> animate;
    group [i++] = analysisd -> accept;
    group [i++] = analysisd -> dismiss;

    XtGetValues (analysisd -> layout, color_args, XtNumber (color_args));
    CreateTabGroup (analysisd -> shell, group, XtNumber (group), 
                    highlight, True);

    XtRealizeWidget (analysisd -> shell);
    SetFocus (analysisd -> title);

    XtSetArg (args [0], XtNwidth, &width);
    XtGetValues (analysisd -> layout, args, 1);
    XtSetArg (args [0], XtNx, &x);
    XtGetValues (analysisd -> help, args, 1);
    UpdateHelpMessage (analysisd -> help, help_message, width - 2 * x);


    /* Add the translations to each widget. */

    AddDeleteWindowProtocol   (analysisd -> shell, "AnalysisDialogAction()");

    XtOverrideTranslations (analysisd -> static_bt,         radio_translations);
    XtOverrideTranslations (analysisd -> transient,         radio_translations);
    XtOverrideTranslations (analysisd -> static_thermal,    radio_translations);
    XtOverrideTranslations (analysisd -> static_incremental,    radio_translations);
    XtOverrideTranslations (analysisd -> static_substitution,    radio_translations);
    XtOverrideTranslations (analysisd -> transient_thermal, radio_translations);
    XtOverrideTranslations (analysisd -> modal,             radio_translations);
    XtOverrideTranslations (analysisd -> spectral,         radio_translations);
    XtOverrideTranslations (analysisd -> title,	       text_translations);
    XtOverrideTranslations (analysisd -> Rk,	       text_translations);
    XtOverrideTranslations (analysisd -> Rm,	       text_translations);
    XtOverrideTranslations (analysisd -> step,	       text_translations);
    XtOverrideTranslations (analysisd -> start,	       text_translations);
    XtOverrideTranslations (analysisd -> stop,         text_translations);
    XtOverrideTranslations (analysisd -> alpha,        text_translations);
    XtOverrideTranslations (analysisd -> beta,         text_translations);
    XtOverrideTranslations (analysisd -> gamma,        text_translations);
    XtOverrideTranslations (analysisd -> iterations,   text_translations);
    XtOverrideTranslations (analysisd -> tolerance,    text_translations);
    XtOverrideTranslations (analysisd -> load_steps,   text_translations);
    XtOverrideTranslations (analysisd -> relaxation,   text_translations);
    XtOverrideTranslations (analysisd -> input_node,   text_translations);
    XtOverrideTranslations (analysisd -> consistent,   toggle_translations);
    XtOverrideTranslations (analysisd -> lumped,       toggle_translations);
    XtOverrideTranslations (analysisd -> left,      command_translations);
    XtOverrideTranslations (analysisd -> right,     command_translations);
    XtOverrideTranslations (analysisd -> dismiss,   command_translations);
    XtOverrideTranslations (analysisd -> accept,    command_translations);
    XtOverrideTranslations (analysisd -> animate,   command_translations);
    XtOverrideTranslations (analysisd -> solve,     command_translations);
    XtOverrideTranslations (analysisd -> help,	    help_translations);
    for (i = 0 ; i < 6 ; i++) {
       XtOverrideTranslations (analysisd -> node[i], text_translations);
       XtOverrideTranslations (analysisd -> dof[i],  toggle_translations);
       XtOverrideTranslations (analysisd -> input_dof[i],  toggle_translations);
    }
    

    /* Add the necessary callbacks. */

    XtAddCallback(analysisd->transient, XtNcallback, SetRadioState, &Button);
    XtAddCallback(analysisd->static_bt, XtNcallback, SetRadioState, &Button);
    XtAddCallback(analysisd->spectral, XtNcallback, SetRadioState, &Button);
    XtAddCallback(analysisd->modal,     XtNcallback, SetRadioState, &Button);
    XtAddCallback(analysisd->transient_thermal, XtNcallback, SetRadioState, &Button);
    XtAddCallback(analysisd->static_thermal,    XtNcallback, SetRadioState, &Button);
    XtAddCallback(analysisd->static_incremental,    XtNcallback, SetRadioState, &Button);
    XtAddCallback(analysisd->static_substitution,    XtNcallback, SetRadioState, &Button);

    for (i = 0 ; i < 6 ; i++)
       XtAddCallback(analysisd -> input_dof [i], XtNcallback, SetInputDOF, NULL);

    XtAddCallback(analysisd->accept, XtNcallback,Accept, (XtPointer) analysisd);
    XtAddCallback(analysisd->dismiss,XtNcallback,Dismiss,(XtPointer) analysisd);

    XtAddCallback(analysisd->consistent, XtNcallback,
                  CheckMassToggle, (XtPointer)analysisd);
    XtAddCallback(analysisd->lumped, XtNcallback,
                  CheckMassToggle, (XtPointer)analysisd);

    XtAddCallback(analysisd->animate, XtNcallback,
                  ExternalCallback, (XtPointer) analysisd);
    XtAddCallback(analysisd->solve, XtNcallback,
                  ExternalCallback, (XtPointer) analysisd);

    XtAddCallback(analysisd->left, XtNcallback, 
                  ShiftNodes, (XtPointer) analysisd);
    XtAddCallback(analysisd->right, XtNcallback, 
                  ShiftNodes, (XtPointer) analysisd);

    XtSetSensitive (analysisd -> static_incremental, False);

    base = 0;

    return analysisd;
}


/************************************************************************
 * Function:	AnalysisDialogPopup					*
 *									*
 * Description:	Pops up the specified material dialog.			*
 ************************************************************************/

void AnalysisDialogPopup (analysisd)
    AnalysisDialog analysisd;
{
    XtPopup (analysisd -> shell, XtGrabNone);
}
