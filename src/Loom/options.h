# ifndef _OPTIONS_H
# define _OPTIONS_H

# include "proto.h"

int ArgsUsed PROTO (( void ));

int GetIntegerOption PROTO ((
   int,
   char	**,
   char	*,
   int	**
));

int GetDoubleOption PROTO ((
   int,
   char	**,
   char	*,
   double **
));

int GetFloatOption PROTO ((
   int,
   char	**,
   char	*,
   float **
));

int GetBooleanOption PROTO ((
   int,
   char **,
   char *,
   int *
));

int GetStringOption PROTO ((
   int,
   char	**,
   char	*,
   char	***
));

int GetSoloIntegerOption PROTO ((
   int,
   char	**,
   char	*,
   int	*
));

int GetSoloDoubleOption PROTO ((
   int,
   char	**,
   char	*,
   double *
));

int GetSoloFloatOption PROTO ((
   int,
   char	**,
   char	*,
   float *
));

int GetSoloStringOption PROTO ((
   int,
   char	**,
   char	*,
   char	**
));

# endif
