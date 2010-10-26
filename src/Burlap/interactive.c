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
 * File:	interactive.c						*
 *									*
 * Description:	This file contains the public and private function	*
 *		definitions for the interactive interface.		*
 ************************************************************************/

# include <stdio.h>
# include <ctype.h>
# include <stdlib.h>
# include <string.h>
# include "error.h"
# include "lexer.h"
# include "allocate.h"
# include "interactive.h"


static const char *start_up_message = "\
This is burlap, copyright 1995 by Jason I. Gobat and Darren C. Atkinson.\n\
This is free software, and you are welcome to redistribute it under certain\n\
conditions, but there is absolutely no warranty.  Type help (\"copyright\")\n\
for details.  Use the -q option to suppress this message.\n\n\
";

# define MaxAliases 64

static struct alias {
    char *alias_name;
    char *real_name;
    int   expanded;
} aliases [MaxAliases];

static int num_aliases;

# define NumDefaults (sizeof (default_aliases) / sizeof (*default_aliases))

static struct {
    const char *alias_name;
    const char *real_name;
} default_aliases [ ] = {
# ifdef READLINE
    {"exit",	"exit (!*)"},
    {"h",	"history (20)"},
    {"help",	"help (\"!$\")"},
    {"ls",	"system (\"ls !*\")"},
    {"quit",	"exit (!*)"},
# else
    {"exit",	"exit (%s)"},
    {"h",	"history (20)"},
    {"help",	"help (\"%s\")"},
    {"ls",	"system (\"ls %s\")"},
    {"quit",	"exit (%s)"},
# endif
};

# ifndef READLINE
static FILE *stream = NULL; 
# endif


# ifndef PROMPT
# define PROMPT "[%u] "
# define CONTINUATION " %u>"
# endif


# ifdef READLINE
# include <readline/history.h>
# include <readline/readline.h>
# include "functab.h"
# include "consttab.h"

static char *completion_generator (char *, int);

static char *keywords [ ] = {
    "break", "do", "else", "end", "for", "function", "global",
    "if", "in", "next", "return", "shared", "then", "while",
    "or", "and", "div", "mod", "not", NULL
};

/************************************************************************
 * Function:	completion_generator					*
 *									*
 * Description:	Completion generator for readline().  If we are inside	*
 *		a string literal, as determined by counting the number	*
 *		of quotes, then filenames are completed.  Otherwise,	*
 *		intrinsic functions and keywords are completed.		*
 ************************************************************************/

static char *completion_generator (char *text, int state)
{
    char      *ptr;
    static int index;
    static int length;
    static int constants;
    static int functions;
    static int filenames;


    if (!state) {
	index = -1;
	functions = 1;
	constants = 1;
	filenames = 0;
	length = strlen (text);

	for (ptr = rl_line_buffer; *ptr; ptr ++)
	    if (*ptr == '"' && (ptr == rl_line_buffer || *(ptr - 1) != '\\'))
		filenames = !filenames;
    }

    if (filenames)
	return filename_completion_function (text, state);

    if (functions) {
	while (++ index < NumIntrinsics)
	    if (!strncmp (text, functab [index].name, length))
		return Strdup (functab [index].name);

	functions = 0;
	index = -1;
    }

    if (constants) {
	if (text [0] == '&')
	    while (++ index < NumConstants)
		if (!strncmp (text, consttab [index].name, length))
		    return Strdup (consttab [index].name);

	constants = 0;
	index = -1;
    }

    while (keywords [++ index])
	if (!strncmp (text, keywords [index], length))
	    return Strdup (keywords [index]);

    return NULL;
}
# endif /* READLINE */


/************************************************************************
 * Function:	compare_aliases						*
 *									*
 * Description:	Compares two aliases.					*
 ************************************************************************/

static int 
compare_aliases(const void *p1, const void *p2)
{
    struct alias *a1;
    struct alias *a2;


    a1 = (struct alias *) p1;
    a2 = (struct alias *) p2;

    return strcmp (a1 -> alias_name, a2 -> alias_name);
}


/************************************************************************
 * Function:	sort_aliases						*
 *									*
 * Description:	Sorts the alias list in place.				*
 ************************************************************************/

static void sort_aliases (void)
{
    qsort (aliases, num_aliases, sizeof (*aliases), compare_aliases);
}


/************************************************************************
 * Function:	parse_alias						*
 *									*
 * Description:	Parses an input line and checks if it is an alias or	*
 *		unalias command.  Note that the parsing is not very	*
 *		sophisticated and no history expansion is done on the	*
 *		input line.						*
 ************************************************************************/

static int parse_alias (char *line)
{
    int  i;
    char alias [1024];
    char real  [1024];


    /* Parse the alias command. */

    if (!strncmp (line, "alias", 5))
	if (!line [5] || isspace (line [5])) {
	    switch (sscanf (line, "%*s%s%*[ \t]%[^\n]", alias, real)) {


	    /* List all aliases. */

	    case -1:
		for (i = 0; i < num_aliases; i ++) {
		    printf ("%s\t", aliases [i].alias_name);
		    printf ("%s\n", aliases [i].real_name);
		}
		break;


	    /* List an alias. */

	    case 1:
		for (i = 0; i < num_aliases; i ++)
		    if (!strcmp (aliases [i].alias_name, alias)) {
			printf ("%s\n", aliases [i].real_name);
			break;
		    }
		break;



	    /* Add or overwrite an alias. */

	    case 2:
		for (i = 0; i < num_aliases; i ++)
		    if (!strcmp (aliases [i].alias_name, alias)) {
			free (aliases [i].real_name);
			aliases [i].real_name = Strdup (real);
			break;
		    }


		if (i == num_aliases) {
		    if (num_aliases < MaxAliases) {
			num_aliases ++;
			aliases [i].alias_name = Strdup (alias);
			aliases [i].real_name = Strdup (real);
			sort_aliases ( );
		    } else
			rterror ("too many aliases");
		}
		break;
	    }

	    line_num ++;
	    return 1;
	}



    /* Parse the unalias command. */

    if (!strncmp (line, "unalias", 7))
	if (!line [7] || isspace (line [7])) {
	    if (sscanf (line, "%*s%s", alias) == 1)
		for (i = 0; i < num_aliases; i ++)
		    if (!strcmp (aliases [i].alias_name, alias)) {
			free (aliases [i].alias_name);
			free (aliases [i].real_name);
			aliases [i] = aliases [-- num_aliases];
			sort_aliases ( );
		    }

	    line_num ++;
	    return 1;
	}


    /* Not a special command. */

    return 0;
}


/************************************************************************
 * Function:	expand_alias						*
 *									*
 * Description:	Expands an alias in the specified text string.  If the	*
 *		readline library is used then history expansion		*
 *		characters work as in csh.				*
 ************************************************************************/

static char *expand_alias (char *text)
{
    int   i;
    int   length;
    int   result;
    char *alias;
    char *expanded;

# ifndef READLINE
    static char temp   [2048];
    static char buffer [2048];
# endif


    for (i = 0; i < num_aliases; i ++)
	aliases [i].expanded = 0;

    for (i = 0; i < num_aliases; i ++) {
	alias = aliases [i].alias_name;
	length = strlen (alias);

	if (!strncmp (text, alias, length) && !aliases [i].expanded)
	    if (!text [length] || isspace (text [length])) {
		aliases [i].expanded = 1;

# ifdef READLINE
		result = history_expand (aliases [i].real_name, &expanded);
		if (result < 0 || result == 2) {
		    rterror ("%s", expanded);
		    free (expanded);
		    break;
		} else {
		    free (text);
		    text = expanded;
		    i = 0;
		    continue;
		}

# else /* READLINE */

		if (text [strlen (text) - 1] == '\n')
		    text [strlen (text) - 1] = 0;

		sprintf (temp, aliases [i].real_name, text + length + 1);
		strcpy (buffer, temp);
		text = buffer;
		expanded = NULL;
		result = 0;
		i = 0;
		continue;

# endif /* READLINE */
	    }
    }

    return text;
}


/************************************************************************
 * Function:	init_interactive					*
 *									*
 * Description:	Initializes the state for interactive mode.		*
 ************************************************************************/

void init_interactive (char *argv0, char *s_file, int q_flag, int a_flag)
{
    int i;


# ifdef READLINE

    char *ptr;

    ptr = (ptr = strrchr (argv0, '/')) ? ptr + 1 : argv0;

    using_history ( );
    rl_readline_name = ptr;
    rl_completion_entry_function = (Function *) completion_generator;
    rl_special_prefixes = "&";

# endif /* READLINE */


    if (!q_flag)
	printf ("%s", start_up_message);

    if (!a_flag) {
	num_aliases = NumDefaults;
	for (i = 0; i < NumDefaults; i ++) {
	    aliases [i].alias_name = Strdup (default_aliases [i].alias_name);
	    aliases [i].real_name = Strdup (default_aliases [i].real_name);
	}
    }


    if (s_file) {
# ifdef READLINE
	if ((rl_instream = fopen (s_file, "r"))) {
	    rl_outstream = fopen ("/dev/null", "w");
	    bfinclude (NULL);
	    fclose (rl_instream);
	    fclose (rl_outstream);
	}
	rl_instream = stdin;
	rl_outstream = stdout;
# else
	if ((stream = fopen (s_file, "r"))) {
	    bfinclude (NULL);
	    fclose (stream);
	}
	stream = stdin;
# endif
    }
}


/************************************************************************
 * Function:	readchar						*
 *									*
 * Description:	Reads and returns the next character from the standard	*
 *		input stream using either getchar() or readline().	*
 ************************************************************************/

int readchar (void)
{
# ifdef READLINE
    int   result;
    char *expansion;
    char  buffer [2048];
    HIST_ENTRY *entry;

    static char *ptr;
    static char *line;
    static char  prompt [32];
    static int   last_line;


    if (ptr && *ptr)
	return *ptr ++;

    if (line) {
	free (line);
	line = ptr = NULL;
	last_line = line_num;
	return '\n';
    }

    sprintf (prompt, last_line == line_num ? CONTINUATION : PROMPT, line_num);

    while ((line = readline (prompt))) {
	if (*line) {
	    if (parse_alias (line)) {
		add_history (line);
		sprintf (prompt, last_line == line_num ? CONTINUATION : PROMPT, line_num);
		continue;
	    }

	    if ((result = history_expand (line, &expansion)))
		printf ("%s\n", expansion);

	    if (result < 0 || result == 2) {
		free (expansion);
		free (line);
		line = NULL;
	    } else {
		free (line);

		if (last_line == line_num) {
		    entry = remove_history (where_history ( ) - 1);
		    sprintf (buffer, "%s %s", entry -> line, expansion);
		    add_history (buffer);
		    free (entry -> line);
		    free (entry);

		} else {
		    add_history (expansion);
		    expansion = expand_alias (expansion);
		}

		ptr = line = expansion;
		return *ptr ++;
	    }
	}
    }

# else /* READLINE */

    static char *ptr;
    static char *expansion;
    static char  line [2048];
    static int   last_line;


    if (ptr && *ptr && *ptr != '\n')
	return *ptr ++;

    if (ptr) {
	ptr = NULL;
	last_line = line_num;
	return '\n';
    }

    ptr = NULL;

    if (stream == stdin)
	printf (last_line == line_num ? CONTINUATION : PROMPT, line_num);

    while (fgets (line, sizeof (line), stream)) {
	if (parse_alias (line)) {
	    if (stream == stdin)
		printf (last_line == line_num ? CONTINUATION : PROMPT, line_num);
	    continue;
	}

	expansion = expand_alias (line);

	if (*expansion && *expansion != '\n') {
	    ptr = expansion;
	    return *ptr ++;
	}

	if (stream == stdin)
	    printf (last_line == line_num ? CONTINUATION : PROMPT, line_num);
    }
# endif /* READLINE */

    return 0;
}


/************************************************************************
 * Function:	print_history						*
 *									*
 * Description:	Prints the last elements of the history list.		*
 ************************************************************************/

int print_history (int n)
{
# ifdef READLINE
    int		 i;
    int		 j;
    HIST_ENTRY **list;


    if ((list = history_list ( ))) {
	i = n ? history_length - n : 0;
	for (i = i < 0 ? 0 : i, j = 0; list [i]; i ++, j ++)
	    printf ("%3d  %s\n", i + history_base, list [i] -> line);

	return j;
    }

# endif /* READLINE */

    return 0;
}
