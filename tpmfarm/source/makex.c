#include <sys/types.h>
#include <sys/stat.h>

/* A function to test permissions on file to 
   executable by user; it is f-callable (hence _)
   requires that a string is nul-terminated
*/

int makex_(const char *path)
{
	int chmod (const char *, mode_t);

/* Use the chmod c-lib function */
	return(chmod(path, S_IEXEC | S_IREAD | S_IWRITE));

}
