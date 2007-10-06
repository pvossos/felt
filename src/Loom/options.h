# ifndef _OPTIONS_H
# define _OPTIONS_H

int ArgsUsed (void);

int GetIntegerOption(int argc, char **argv, char *name, int **opt);

int GetDoubleOption(int argc, char **argv, char *name, double **opt);

int GetFloatOption(int argc, char **argv, char *name, float **opt);

int GetBooleanOption(int argc, char **argv, char *name, int *opt);

int GetStringOption(int argc, char **argv, char *name, char ***opt);

int GetSoloIntegerOption(int argc, char **argv, char *name, int *opt);

int GetSoloDoubleOption(int argc, char **argv, char *name, double *opt);

int GetSoloFloatOption(int argc, char **argv, char *name, float *opt);

int GetSoloStringOption(int argc, char **argv, char *name, char **opt);

# endif
