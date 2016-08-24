/* 
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
| D0library UNIX utilities                                           FERMILAB |
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#

  make_copy_fz_rcp.c

  Modifies the standard $d0util/files/copy_fz_rcp file to create one which
  will concatenate several output files from GEANT runs.

  The output files must exist in a directory structure like the following:

  /.../run-root/0
               /1
               /2
               /3

  And so on where each directory [0-9]* contains an output file with the same
  name that is the output of the GEANT run.  The directories names must be
  numerical but need not be in sequential order (although this makes life
  easier).  To run the program:

  % make_copy_fz_rcp <run-root> <output-file> <concat-file> <range>

  where <run-root> is the full path of the run-root directory shown above,
  <output-file> is the name of the file which appears in every run.  These
  output files are concatenated into <concat-file>.  <range> is a space-
  delimited list of directories to be included in the concatentation, for
  instance:  0-9 14 20-200.

  The output RCP file is placed in <concat-file>.  If any of the directories
  requested in <range> does not exist, a message to that effect is placed
  in the RCP file itself and not echoed to the display.  make_copy_fz_rcp will
  not run if <output-file> already exists.  Remove it and then run the
  program.

  Date:         Changes:
  02-14-94      Coded by R. Robert Hentzel and ported to ULTRIX and IRIX
  16-03-04      sss - fix return type.

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
*/

#include <stdio.h>
#include <stdlib.h>

#ifdef linux
#include <string.h>
#endif

#ifdef ultrix
#include <string.h>
#endif

#ifdef sgi
#include <strings.h>
#endif

int main (int argc, char *argv[]) {

  char z[160],fzfile[160],test[160],outfile[160];
  int i,j,a,b;
  char *p;
  FILE *f,*f2,*o;

/* Use $d0util/files/copy_fz_rcp if $d0util is defined; otherwise try the
   hardcoded value /d0library/util/files/copy_fz_rcp */

#ifdef ultrix
  if (getenv("d0util")) strcpy(fzfile,getenv("d0util"));
  else strcpy(fzfile,"/d0library/util");
  if (getenv("PWD")) strcpy(outfile,getenv("PWD"));
  else strcpy(outfile,".");
#endif

#ifdef sgi
  getenv("d0util",fzfile);
  if (fzfile[0]==0) strcpy(fzfile,"/d0library/util");
  getenv("PWD",outfile);
  if (outfile[0]==0) strcpy(outfile,".");
#endif

  strcat(fzfile,"/files/copy_fz.rcp");
  strcat(outfile,"/copy_fz_rcp");

/* Check to see if <output-file> already exists; exit if it does. */

  if (o=fopen(outfile,"r")) {
    fprintf(stderr,"%s: %s already exists.\n",argv[0],outfile);
    exit(1);
  };

  o = fopen(outfile,"w");
  f = fopen(fzfile,"r");

/* At least four arguments are necessary for valid parameters; if less
   print help message. */

  if (argc > 4) {
    fprintf(o,"! copy_rz_rcp file created automatically by make_rcp\n");

/* Copy the RCP file until we reach the INP_FORM fields */

    fgets(z,160,f);
    while (strncasecmp(z,"INP_FORM",8)) {
      fprintf(o,"%s",z);
      fgets(z,160,f);}
    fgets(z,160,f);

/* Replace the *_FORM fields with 'NATIVE' */

    fprintf(o,"INP_FORM    'NATIVE'    ! Input file format (changed from default)\n");
    fprintf(o,"OUTP_FORM   'NATIVE'    ! Output file format (changed from default)\n");

/* Copy the RCP file until we get to \ARRAY FILE_LIST */

    do {
      fgets(z,160,f);
      fprintf(o,"%s",z);}
    while (strncasecmp(z,"\\ARRAY FILE_LIST",16));

/* Format each element of <range> as an x-y list.  Example
   0-9 14 16 20-200 366 --> 0-9 14-14 16-16 20-200 366-366
   and then deal with each the same way, looping from first to last. */

    for (i=4;i<argc;i++) {
      strcpy(z,argv[i]);
      if (p=index(z,'-')) {
        *p=0;
        a=atoi(z);
        b=atoi(p+1);
        }
      else
        a=b=atoi(z);
      for(j=a;j<=b;j++) {
        sprintf(test,"%s/%d/%s",argv[1],j,argv[2]);
        if (f2=fopen(test,"r")) {                        /* Does it exist? */
          fclose(f2);
          fprintf(o,"'%s/%d/%s' '%s' 000 999\n",argv[1],j,argv[2],argv[3]);
        }                    /* Print in proper RCP formatting */
        else fprintf(o,"! ERROR %s/%d/%s is not readable\n",argv[1],j,argv[2]);
      };
    };
    fprintf(o,"'END'\n");

/* Copy the rest until END line */

    do fgets(z,160,f); while (strncasecmp(z,"! 'END'",7));
    while (fgets(z,160,f) != NULL) fprintf(o,"%s",z);
    fclose(f);
    fclose(o);
  }
  else {

/* The user requires instruction... */

    printf("make_rcp -- automatic copy_fz_rcp generator for concatenation\n");
    printf("Usage:  make_rcp <parent-dir> <filename> <output> <range>\n");
    printf("  <parent-dir> root directory of numbered run directories\n");
    printf("  <filename> filename in each run directory of events\n");
    printf("  <output> path and filename of concatenated event file\n");
    printf("  <range> space-delimited list of numbers and ranges to concatenate.\n");
    printf("Example:  make_rcp /scratch/d0 t2bx160.gean ~/output 1-5 10 12-16\n");
  }
  return 0;
}

