/*
 *-
 *-   Purpose and Methods: Rewrite a D0DAD new_runs_XXXXXX.lis file to 
 *-      contain only a single reference for contiguous event sets. 
 *-      All files listed in new_runs_XXXXXX.lis must have standard D0
 *-      naming form:   directory:name.ext (or directory/name.ext) and
 *-      the name and extension fields for parts from the same run must
 *-      have the same sizes.
 *-
 *-   Inputs  :
 *-   Outputs :
 *-   Controls:
 *-
 *-   Created  28-Dec-1994   John D. Hobbs
 *-
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define NENTRIES_MAX 2000
#define NPARTS_MAX  99
#define COMMENT   12000  /* 99 parts * 120 chars/file */
#define FNAME_MAX 12000

#define DEFAULT     0   /* Operation arising from comparing two records. */
#define AUGMENT     1
#define REPLACE     2
#define SUBSET      3
#define ERROR_MATCH 4

int    nin;                 /* Number of entries in current file. */
struct entry_tag            /* Array of run/evtnum triplets for current file.*/
{ 
  int runnum,evtnum[2];
  char comment[COMMENT];
} *entries;

#define EVTMIN(i,j) ( entries[i].evtnum[0]<entries[j].evtnum[0] ?  entries[i].evtnum[0] : entries[j].evtnum[0] )
#define EVTMAX(i,j) ( entries[i].evtnum[1]>entries[j].evtnum[1] ?  entries[i].evtnum[1] : entries[j].evtnum[1] )

main(int argc, char **argv, char **envp)
{
  int i;

  /* Setup the memory buffer. */
  entries = malloc( NENTRIES_MAX*sizeof(struct entry_tag) );
  if( (void *)entries == (void *)NULL ) {
    fprintf(stderr,"Fatal error: Could not allocate memory\n");
    return(1);
  }


  /* If no files are given, just process using standard input. */
  if( argc==1 && readin("stdin")>0 ) {
    coallesce();
    writeout("stdout");
  }

  /* Process each file in turn. */
  for( i=1 ; i<argc ; i++ ) if( readin(argv[i])>0 ) {
    coallesce();         
    writeout(argv[i]);
  }

  return(0);
}

/*
 *-   Purpose and Methods: Read the contents of the NEW_RUNS_XXXXXX.LIS file
 *-     given as fname. If fname="stdin", do not do the open
 *-
*/
int readin(const char *fname)
{
  FILE *fp;
  char line_in[FNAME_MAX],*incomment;
  struct entry_tag *data=entries;

  /* Setup the input file pointer. */
  fp=stdin;
  if( strcmp(fname,"stdin")!=0 && (fp=fopen(fname,"r")) == (FILE *)NULL ) {
    fprintf(stderr,"Cannot open: %s for input\n",fname);
    return(0);
  }  

  /* Do the processing. */
  nin = 0;
  while( fgets(line_in,FNAME_MAX,fp) != (char *)NULL && nin<NENTRIES_MAX ) {
    sscanf(line_in,"%d %d %d",&data->runnum,&data->evtnum[0],&data->evtnum[1]);
    data->comment[0]='\0';
    incomment = strchr(line_in,'!');
    if( incomment != (char *)NULL ) {
      strncpy(data->comment,incomment,COMMENT);
      incomment = data->comment;
      while((incomment=strchr(incomment,'\n'))!=(char *)NULL) *incomment='\0';
    }
    data++;
    nin++;
  }

  /* Release the input file */
  if( fp != stdin ) fclose(fp);
  return(nin);
}


/*
 *-   Purpose and Methods: Coallesce entries for adjacent events within in
 *-     run into a single section.
 *-
*/
coallesce()
{
  char *incomment;
  int test(const void *, const void *),i=0,j=1,k;
  char compare( int, int);

  qsort( (void *)entries, nin, sizeof(struct entry_tag), test);
  while(  i<(nin-1) ) switch( compare(i,j) ) {

    /* Append new record to end of old... */
    case AUGMENT:                
      entries[i].evtnum[1] = entries[j].evtnum[1];
      incomment = strchr(entries[j].comment,'!');
      if( incomment != (char *)NULL ) {
        while( (*incomment=='!' || *incomment==' ') &&
          *incomment!='\0') incomment++;
	strncat(entries[i].comment,",",COMMENT-1);
        strncat(entries[i].comment,incomment,COMMENT-1);
      }
      entries[j].runnum = -1;
      j++;
      break;

    /* Keep the most recent of all occurances. */
    case REPLACE:
      merge_names(entries[i].comment,entries[j].comment);
      entries[j].runnum = -1;
      j++;
      break;

    /* The new record has file(s) forming a subset of current. */
    /* Keep the largest event range, and newer files...        */
    case SUBSET:
      entries[i].evtnum[0]=EVTMIN(i,j);
      entries[i].evtnum[1]=EVTMAX(i,j);
      merge_names(entries[i].comment,entries[j].comment);
      entries[j].runnum = -1;
      j++;
      break;

    /* Do nothing, just increment the pointers. */
    case DEFAULT:
    default:
      i++; 
      while( entries[i].runnum == (-1) ) i++; 
      j=i+1; 
      break;

    case ERROR_MATCH:
      fprintf(stderr,"Logic error for records %d and %d\n",i,j);
      fprintf(stderr,"  Record %d: Run/Evt1/Evt2: %d/%d/%d\n",i,
        entries[i].runnum,entries[i].evtnum[0],entries[i].evtnum[1]);
      fprintf(stderr,"  Record %d: Run/Evt1/Evt2: %d/%d/%d\n",j,
        entries[j].runnum,entries[j].evtnum[0],entries[j].evtnum[1]);
      exit(2);
  }
}


/*
 *-   Purpose and Methods: Write the coallesced records to the file on fp.
*/
writeout(const char *fname)
{
  struct entry_tag *data=entries;
  char   oldname[133],*crlf;
  FILE   *fp;
  int    i;

  /* Setup the new output file. */
  fp=stdout;
  if( strcmp(fname,"stdout")!=0 ) {
    strncpy(oldname,fname,128);
    strncat(oldname,".old",132);
    rename(fname,oldname);
    if( (fp=fopen(fname,"w")) == (FILE *)NULL ) {
      fprintf(stderr,"Cannot open: %s for output\n",fname);
      return(1);
    }
  }

  /* Do the write. */
  for(i=0 ; i<nin ; i++) {
    if( data->runnum!=(-1)) {
      crlf=data->comment;
      while( (crlf=strchr(crlf,'\n')) != (char *)NULL ) *crlf='\0';
      fprintf(fp,"%7d %7d %7d %s\n",data->runnum,data->evtnum[0],
           data->evtnum[1],data->comment);
    }
    data++;
  }

  /* Clean up. */
  close(fp);
}

/*
 *-   Purpose and Methods: Quicksort comparison routine.
 *-
*/
int test(const void *d1, const void *d2)
{
  struct entry_tag *ele1=(struct entry_tag *)d1,
                   *ele2=(struct entry_tag *)d2;

  if( ele1->runnum < ele2->runnum ) return( -1 );
  if( ele1->runnum > ele2->runnum ) return( 1 );
  if( ele1->evtnum[0] < ele2->evtnum[0] ) return( -1 );
  if( ele1->evtnum[0] > ele2->evtnum[0] ) return( 1 );
  return( 0 );
}

/* 
 *-  Decide how to process the current pair of entries. 
 *-
*/
char compare( int i, int j ) {

  /* Must be 'live' and have same run number to consider matching. */
  if( entries[i].runnum == (-1) ) return(DEFAULT);
  if( entries[i].runnum != entries[j].runnum ) return(DEFAULT);
  if( entries[i].evtnum[1] < (entries[j].evtnum[0]-10) ) return(DEFAULT);
  if( j >= nin ) return(DEFAULT);

  /* Have live entries in same run.  Categorize based on exact duplicate
     event range, subset or additional events.  In sort, for entries with
     same run, the earlier entry will have a lower initial event number.
  */
  if( (entries[i].evtnum[0] == entries[j].evtnum[0]) &&
      (entries[i].evtnum[1] == entries[j].evtnum[1]) ) return(REPLACE);
  if( (entries[i].evtnum[0] <= entries[j].evtnum[0]) &&
      (entries[i].evtnum[1] >= entries[j].evtnum[1]) ) return(SUBSET);
  if( ( entries[i].evtnum[1] > (entries[j].evtnum[0]-10)) &&
      ( entries[i].evtnum[1] < entries[j].evtnum[0]) ) return(AUGMENT);
  if( entries[i].evtnum[1] > (entries[j].evtnum[0]-10) ) return(SUBSET);

  /* This is an undefined circumstance -- should never happen.  Crash it. */
  return(ERROR_MATCH);

}

/*
 *-
 *-   Purpose and Methods: Merge two file-name lists into one.
 *-
 *-   Inputs  :
 *-   Outputs :
 *-   Controls:
 *-
 *-   Created  27-Jun-1995   John D. Hobbs
 *-
*/
merge_names(char *s1, char *s2)
{
  char temp[FNAME_MAX];
  char both1[128],both2[128],name1[128],name2[128],ext1[128],ext2[128];
  char full_field[128],field1[128],field2[128];
  char *fn1,*fn2,*ptemp,*ts1,*ts2;
  int  done[NPARTS_MAX],i,npart,done_all;

  /* Check for simple identity. */
  if( !strcmp(s1,s2) ) return(0);

  /* Set up the merge. (Skip comment character) */
  fn1 = s1; fn1++ ; ptemp = fn1;
  fn2 = s2; fn2++ ;
  for( i=0 ; i<NPARTS_MAX ; i++ ) done[i] = 0;

  /* Check all entries with identical names. */
  while( extract_field(full_field,&fn1) ) {
    extract_name_and_extension(name1,ext1,both1,full_field);
    npart=0;
    while( extract_field(full_field,&fn2) ) {
      extract_name_and_extension(name2,ext2,both2,full_field);
      if( strcmp(name1,name2) == 0 && strcmp(ext1,ext2) != 0 ) {
	ts1 = strrchr(ext1,'_');
	ts2 = strrchr(ext2,'_');
        /* Check to see if should use the newer name. */
	if( (strcmp(ts1,ts2)<0) || (strcmp(ts1,"_NONEX")==0) ) {
	  ts1 = strchr(ptemp,'.');
	  if( strcmp(ts1,"_NONEX") != 0 )  /* Just superceding. */
	    for( i=0 ; i<strlen(ext2) ; i++ ) ts1[i+1]=ext2[i];
	  else {                    /* Old bug fixup - overwrite next field? */
	    strcpy(temp,fn1);
	    for( i=0 ; i<strlen(ext2) ; i++ ) ts1[i+1]=ext2[i];
            if( strlen(temp)>0 ) ts1[i++]=',';
	    ts1[i]='\0';
	    strcat(s1,temp);
	  }
        }
	done[npart]=1;
      }
      else if( strcmp(both1,both2) == 0 ) done[npart]=1; 
      npart += 1;
    }
    fn2 = s2; fn2++ ;
    ptemp = fn1;
  }

  /* Check for files not in the list. */
  /* Set up the initial state. */
  npart=0;
  temp[0]='\0';
  fn1 = s1;
  fn2 = s2;
  done_all = 3;     /* Bitmask.  Set to 0 when both lists are exhausted. */
  if( !extract_field(field1,&fn1) ) done_all &= ~1; 
  else extract_name_and_extension (name1,ext1,both1,field1);
  while( (i=extract_field(field2,&fn2)) && done[npart++] ) ;
  if( !i ) done_all &= ~2;
  else extract_name_and_extension(name2,ext2,both2,field2);

  /* Do the looping until both lists completed. */
  while( done_all != 0 ) switch( select_field(name1,name2,done_all) ) {
    case 1:
      if(strlen(temp)==0) strcat(temp,"!");
      if(strlen(temp)>2) strcat(temp,",");
      if( field1[0] == '!' ) for( i=0 ; i<strlen(field1) ; i++ ) 
	field1[i]=field1[i+1];
      strcat(temp,field1);
      if( !extract_field(field1,&fn1) ) done_all &= ~1; /* Get next field */
      else extract_name_and_extension(name1,ext1,both1,field1);
      break;
    case 2:
      if(strlen(temp)==0) strcat(temp,"!");
      if(strlen(temp)>2) strcat(temp,",");
      if( field2[0] == '!' ) for( i=0 ; i<strlen(field2) ; i++ ) 
	field2[i]=field2[i+1];
      strcat(temp,field2);
      while( (i=extract_field(field2,&fn2)) && done[npart++] );/* Next field */
      if( !i ) done_all &= ~2;
      else extract_name_and_extension(name2,ext2,both2,field2);
      break;
  }

  /* All done. */
  strcpy(s1,temp);
  return(1);
}

/*
 *-
 *-   Purpose and Methods: Extract elements of a comma separted list.
 *-
 *-   Inputs  :
 *-   Outputs :
 *-   Controls:
 *-
 *-   Created  28-Jun-1995   John D. Hobbs
 *-
*/
extract_field( char *full_field, char **listptr )
{
  char   *end,*list;

  /* Done processing...? */
  list = *listptr;
  if( strlen(list) <= 0 ) return(0);

  /* Extract the current field into local storage... */
  end = strchr(list,',');  
  if( end != (char *)NULL ) {    /* Not the last element in the list. */
    *end = '\0';
    strcpy(full_field,list);
    *end = ',';
    *listptr = end + 1;
  }
  else {                         /* Last element in the list. */
    strcpy(full_field,list);  
    *listptr = list + strlen(list);
  }

  return(1);
}

/*
 *-
 *-   Purpose and Methods: 
 *-
 *-   Inputs  :
 *-   Outputs :
 *-   Controls:
 *-
 *-   Created  28-Jun-1995   John D. Hobbs
 *-
*/
extract_name_and_extension(char *name, char *ext, char *both, char *full_field)
{
  char *start,*end;

  /* Defaults for missing fields. */
  ext[0]='\0';
  name[0]='\0';
  both[0]='\0';

  /* Get positions delimiting name field. */
  end = (char *)NULL;
  end = strrchr(full_field,'.');
  start = strchr(full_field,']');
  if( start == (char *)NULL ) start = strrchr(full_field,'/');
  if( start == (char *)NULL ) start = strrchr(full_field,':');
  if( start == (char *)NULL ) start = full_field;
  else start += 1;

  /* Copy fields... */
  strcpy(both,start);                          /* The name and extension. */
  if( end != (char *)NULL ) *end = '\0';       /* The name only. */
  strcpy(name,start);
  if( end != (char *)NULL ) *end = '.';
  if( end != (char *)NULL ) strcpy(ext,end+1); /* The extension. */
  
  /* Convert to uppercase. */
  upcase(both);
  upcase(name);
  upcase(ext);
}

/*
 *-
 *-   Purpose and Methods: Convert a text string to uppercase.
 *-
 *-   Inputs  :
 *-   Outputs :
 *-   Controls:
 *-
 *-   Created  28-Jun-1995   John D. Hobbs
 *-
*/
upcase(char *sptr)
{
  int i;
  for( i=0 ; i<strlen(sptr) ; i++ ) sptr[i]=toupper(sptr[i]);
}

/*
 *-
 *-   Purpose and Methods: 
 *-
 *-   Inputs  :
 *-   Outputs :
 *-   Controls:
 *-
 *-   Created  28-Jun-1995   John D. Hobbs
 *-
*/
select_field(char *f1, char *f2, int flag)
{
  /* Check for possibly completed lists. */
  if( !flag ) return(0);       /* If all are done, do nothing. */
  if( !(flag&1) ) return(2);   /* If first is finished, use second. */
  if( !(flag&2) ) return(1);   /* If second is finished, use first. */

  /* Both lists are active, choose the correct one. */
  if( strcmp(f1,f2)<0 ) return(1);
  else return(2);
}

