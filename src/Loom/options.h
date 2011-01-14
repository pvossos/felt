# ifndef _OPTIONS_H
# define _OPTIONS_H

#ifdef __cplusplus
extern "C" {
#endif 

/*----------------------------------------------------------------------*/

int ArgsUsed (void);

int GetIntegerOption(int argc, char **argv, const char *name, int **opt);

int GetDoubleOption(int argc, char **argv, const char *name, double **opt);

int GetFloatOption(int argc, char **argv, const char *name, float **opt);

int GetBooleanOption(int argc, char **argv, const char *name, int *opt);

int GetStringOption(int argc, char **argv, const char *name, char ***opt);

int GetSoloIntegerOption(int argc, char **argv, const char *name, int *opt);

int GetSoloDoubleOption(int argc, char **argv, const char *name, double *opt);

int GetSoloFloatOption(int argc, char **argv, const char *name, float *opt);

int GetSoloStringOption(int argc, char **argv, const char *name, char **opt);

/*----------------------------------------------------------------------*/

#ifdef __cplusplus
}
#endif 

# endif
