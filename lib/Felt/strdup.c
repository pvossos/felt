/* strdup.c -- duplicate string contents using malloc(3)
 *
 * Written by reading the System V Interface Definition, not the code.
 *
 * Totally public domain.
 *
 */

# ifdef NEED_STRDUP
char
*strdup(str)
char *str;
{
	extern char *malloc(), *strcpy();

	char	*copy = malloc(strlen(str) + 1);

	return(strcpy(copy, str));
}
/* strdup.c ends here */
# endif
