#include <sys/types.h>
#include <stdio.h>
#include <string.h>

#define MAXFILE 1000  /* Max number of files on an input tape */

/* Given the filename returns whether it is on tape or not and what is its
sequence number; should be linked together with inspool_l.f
Kirill Denisenko, 09/22/93
*/
int sequence_check_(char *infile, char *inlabel, int *last_seq, int *seque)
{

static int tot_seq = 0;
static int repeat = 1;
FILE *fcontents;
char filename[200];
char name1[132], label1[70];
static char name[MAXFILE][132];
static int  seq[MAXFILE];
char *getenv(char *), *prodir;
char *label, *namefile;
int i;

/* String arguments are from F - hence non-terminated */
strncpy(label1,inlabel,30);
label = strtok(label1," ");
strncpy(name1,infile,90);
namefile = strtok(name1," ");
if ( repeat ) {
	repeat = 0;
/* First open a file with tape contents and read it in */
	if ( (prodir = getenv("PRODIR")) == NULL ) {
		printf("PRODIR env is not defined\n");
		exit(-1);
	}
	strcpy(filename,prodir);
	strcat(filename,"/history/tapes_moved.");
	strcat(filename,label);
	if( (fcontents = fopen ( filename, "r")) == NULL ) { 
		printf("File %s not found, use defaults\n",filename);
	}
	else {
/* Now read the contents                                */
		while ( fscanf(fcontents, "%s %d %s %*d", 
                label, &seq[tot_seq], &name[tot_seq]) != EOF )
			 tot_seq++;
	}
}

if ( tot_seq == 0 ) {
	*seque = 0;
	*last_seq = 0;
	return(1);
}

/* Always set it to tot_seq  */
*last_seq = seq[tot_seq-1];
*seque = 0;

/* Now check that the filename "namefile" is in the list           */
for ( i = 0; i<tot_seq; i++ ) {
	if ( strstr(namefile,name[i]) != NULL ) {
		*seque = seq[i];
		return(1);
	}
}
/* File not found */
return(0);
}
