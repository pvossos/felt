# include <stdio.h>
# include <stdlib.h>
# include <string.h>
# include <ctype.h>
# include "allocate.h"
# include "proto.h"
# include "options.h"
# include "error.h"


# define streq(a,b)	 !strcmp(a,b)

static int args_used = 0;

# ifdef UseFunctionPrototypes
static int GetArgNumber (int argc, char *argv[], char *name)
# else
static int GetArgNumber (argc, argv, name)
   int	argc;
   char	*argv [ ];
   char	*name;
# endif
{
   char	buffer [256];
   int	i;
   int  n;

   sprintf (buffer,"-%s", name);      

   n = -1;
   for (i = 1 ; i < argc ; i++) 
      if (streq (argv [i], buffer)) {
         n = i;
         break;
      }
 
   return n;
}

# ifdef UseFunctionPrototypes
static int CountArgs (int argc, char *argv[], int n)
# else
static int CountArgs (argc, argv, n)
   int	 argc;
   char	*argv [ ];
   int	 n;
# endif
{
   int	 i;
   int	 c;

   c = 0;

   for (i = n ; i < argc ; i++) 
      if (argv [i][0] == '-' && !isdigit(argv [i][1]) && argv [i][1] != '.') {
         c = i - n;
         break;
      }

   if (i == argc)
      c = argc - n;

   return c;
}

int ArgsUsed ( )
{
   return args_used;
}

int GetIntegerOption(argc, argv, name, opt)
   int	 argc;
   char	*argv [ ];
   char	*name;
   int	**opt;
{
   int		n;
   int		c;
   int		i;

   n = GetArgNumber (argc, argv, name);
   if (n == -1) 
      return 0;

   c = CountArgs(argc, argv, n+1);

   *opt = Allocate(int, c);
   if (*opt == NULL)
      Fatal("could not allocate memory for argument options");

   for (i = 0  ; i < c ; i++) 
      (*opt) [i] = atoi (argv [i + n + 1]);

   args_used = args_used + c + 1;

   return c;
}

int GetDoubleOption(argc, argv, name, opt)
   int	     argc;
   char	    *argv [ ];
   char	    *name;
   double  **opt;
{
   int		n;
   int		c;
   int		i;

   n = GetArgNumber (argc, argv, name);
   if (n == -1) 
      return 0;

   c = CountArgs(argc, argv, n+1);

   *opt = Allocate(double, c);
   if (*opt == NULL)
      Fatal("could not allocate memory for argument options");

   for (i = 0  ; i < c ; i++) 
      (*opt) [i] = atof (argv [i + n + 1]);

   args_used = args_used + c + 1;

   return c;
}

int GetFloatOption(argc, argv, name, opt)
   int	     argc;
   char	    *argv [ ];
   char	    *name;
   float   **opt;
{
   int		n;
   int		c;
   int		i;

   n = GetArgNumber (argc, argv, name);
   if (n == -1) 
      return 0;

   c = CountArgs(argc, argv, n+1);

   *opt = Allocate(float, c);
   if (*opt == NULL)
      Fatal("could not allocate memory for argument options");

   for (i = 0  ; i < c ; i++) 
      (*opt) [i] = atof (argv [i + n + 1]);

   args_used = args_used + c + 1;

   return c;
}

int GetBooleanOption(argc, argv, name, opt)
   int	 argc;
   char	*argv [ ];
   char	*name;
   int	*opt;
{
   int		n;
   int		c;

   n = GetArgNumber (argc, argv, name);
   if (n == -1) {
      *opt = 0;
      return 0;
   }

   c = CountArgs(argc, argv, n+1);
   if (c > 0)
       return -1;

   *opt = 1;

   args_used = args_used + c + 1;

   return c;
}

int GetStringOption(argc, argv, name, opt)
   int	 argc;
   char	*argv [ ];
   char	*name;
   char	***opt;
{
   int		n;
   int		c;
   int		i;
 
   n = GetArgNumber (argc, argv, name);
   if (n == -1) 
      return 0;
 
   c = CountArgs(argc, argv, n+1);

   *opt = Allocate(char *, c);
   if (*opt == NULL)
      Fatal("could not allocate memory for argument options");

   for (i = 0  ; i < c ; i++)
      (*opt) [i] = strdup (argv [i + n + 1]);

   args_used = args_used + c + 1;

   return c;
}

int GetSoloIntegerOption(argc, argv, name, opt)
   int	 argc;
   char	*argv [ ];
   char	*name;
   int	*opt;
{
   int		n;
   int		c;

   n = GetArgNumber (argc, argv, name);
   if (n == -1) 
      return 0;

   c = CountArgs(argc, argv, n+1);
   if (c != 1)
      return -1;

   *opt = atoi (argv [n + 1]);

   args_used = args_used + c + 1;

   return c;
}

int GetSoloFloatOption(argc, argv, name, opt)
   int	    argc;
   char	   *argv [ ];
   char	   *name;
   float   *opt;
{
   int		n;
   int		c;

   n = GetArgNumber (argc, argv, name);
   if (n == -1) 
      return 0;

   c = CountArgs(argc, argv, n+1);
   if (c != 1)
      return -1;

   *opt = atof (argv [n + 1]);

   args_used = args_used + c + 1;

   return c;
}

int GetSoloDoubleOption(argc, argv, name, opt)
   int	    argc;
   char	   *argv [ ];
   char	   *name;
   double  *opt;
{
   int		n;
   int		c;

   n = GetArgNumber (argc, argv, name);
   if (n == -1) 
      return 0;

   c = CountArgs(argc, argv, n+1);
   if (c != 1)
      return -1;

   *opt = atof (argv [n + 1]);

   args_used = args_used + c + 1;

   return c;
}

int GetSoloStringOption(argc, argv, name, opt)
   int	 argc;
   char	*argv [ ];
   char	*name;
   char	**opt;
{
   int		n;
   int		c;
 
   n = GetArgNumber (argc, argv, name);
   if (n == -1) 
      return 0;
 
   c = CountArgs(argc, argv, n+1);
   if (c != 1)
      return -1;

   *opt = strdup (argv [n + 1]);

   args_used = args_used + c + 1;

   return c;
}
