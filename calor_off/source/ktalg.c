/***********************************************************
 *
 *  ktalc.c
 *
 *  Purpose & Methods:
 *
 *  The routines in this file implement the Kt jet clustering
 *  algorithm.
 *
 *  Created: 1-jan-1995 Rich Astur (date may not be right?)
 *
 *  Modified: Oct-1995 Gordon Watts & Brad Abbott
 *             Bring into line with D0 C coding standards
 *  Modified 19-Nov_1995     Fix so compiles with VAXC compiler
 *                           Gordon Watts
 *  Modified 5-Dec_1995      Brad Abbott  allow for emfrac,chfrac
 *                                        etc, for ktjets
 *
 **********************************************************/

/*
	We  have -ncolumn- 4-vectors. For each one, we will make
	an integer array which holds the indices of all the other
	4-vectors that are with D of it. So column 1 is the array
	that holds all the 4-vectors that are within D of vector 1.
	We only include entries above the column (i.e. column 5 only
	has enties above 5, or else we double count and we dont need
	to.

	int **pcol : points to the array on integer arrays. One for
					 each column.
	int *ncol  : is an array which holds the number of entries
					 in each column. Start this at 0.
	*/
/* include */
#include <stdlib.h>
#include <stdio.h>
#include <math.h>

/* Macro Definitions */
#define min( a1, a2 )	( (a1) < (a2) ? (a1) : (a2) )
#define max( a1, a2 )	( (a1) > (a2) ? (a1) : (a2) )

#ifdef vms
#  define FNAME(a) a
#else
#  define FNAME(a) a##_
#endif

/* DEFINE statements */
#define JPOINT		1
#define JPX			2
#define JPY			3
#define JPZ			4
#define JE			5
#define JET			6
#define JETA		7
#define JPHI		8
#define KVEC_NCELL  9
#define KVEC_EM     10
#define KVEC_ICD    11
#define KVEC_FH     12
#define KVEC_CELL_EM 13
#define KVEC_CELL_ICD 14
#define KVEC_CELL_FH 15
#define NREP_KVEC   15
#define NREP_KMAP   3
#define PI			3.141593
#define TWOPI		6.283185

/*
	function prototypes
	*/
float fabsf (float a) {return a > 0 ? a : -a; }
void FNAME(ktclust_init)(int *ncolumns, float *dpar, long *pkvec, long *pmap,
					long *pklis, int *ietdef, int *ietadef ); /* init */
void FNAME(ktclust_fill)( void );		/* fill table */
static void ktclust_fill_col( int icolumn); /* fill column */
void FNAME(ktclust_do)( void );		/* do  clustering */
void FNAME(ktclust_done)( void );				/* done. free memory.*/
static float ktmkl( int ivect, int jvect );	/* dij */
static void ktclust_merge_cells( int ivect, int jvect ); /* merge i and j */
extern void FNAME(etoeta)( float *fourvec, float *phi, float *theta, float *eta );
/*extern float ktjet_mkl( int *ivect, int *jvect );*/
/* these are automatically global because they are outside
	 of any routine */
static float D_SEP;	  /* separation parameter */
static int etdef;		  /* ET definition 1=vector, 2=esintheta, 3=scalar */
static int etadef;		  /* ETA,PHI definition 1=e weighted, 2=et weighted */
static int **pcol;		  /* pointer to column array , each column holds list of clusters*/
static float **pmkl;	  /* pointer to mkl array, each column holds dij's for clusters */
static int *ncol;		  /* array of # of entries for each column */
static int *ncolid;	  /* id of this column (which cluster is it)*/
static int *nclloc;	  /* which column is this cluster in? */
static float *colktmin; /* kt minimum of this column */
static int *jcolktmin;  /* cluster j of minimum i<j */
static int nclusters;	  /* number of 4-vectors total (to start) */
static int nsize;		  /* size of arrays (nclusters) */
static int nactive;	  /* number of columns we're using */
/*int nrep;*/		  /* repetition number of kvec bank */
static long *lkvec;	  /* pointer to kvec bank */
static long *lkmap;		 /* pointer to kmap bank */
static long *lklis;     /* pointer to accounting bank */
static int nlis;		  /* which one are we on? */



/***********************************************************/
void FNAME(ktclust_init)(int *ncolumns, float *dpar, long *pkvec, long *pmap,
					long *pklis, int *ietdef, int *ietadef )
{
	int i;
	
	lklis	=	pklis;		/* point to KHIS bank */
	lkvec	=   pkvec;		/* point to KVEC bank */
	lkmap	=   pmap;		/* point to KMAP bank */
	D_SEP	=   *dpar;		/* KT separation parameter D */
	etdef	=   *ietdef;		/* ET def */
	etadef	=	*ietadef;	/* ETA,PHI def */
	nlis	=   *ncolumns-1;  /* count steps and enter into KHIS as we do */
	
/* Use a zebra like array of both floats and ints. Must be the same size.
   */
	if ( sizeof(long) != sizeof(float) )
		exit(32);
/*
	Do initialization. Allocate space for all the arrays we
	need. Note we  need   columns and entries
	*/
	nclusters	= *ncolumns; 	/* number to start */
	nsize		= *ncolumns;    /* size to make arrays  */
	nactive		= *ncolumns;    /* number left */

/* column array pointer */
	pcol	= ( int ** ) malloc( sizeof( int *)* nsize );
	pmkl	= ( float **) malloc( sizeof( float *)*nsize );
	ncol	= ( int *)   malloc( sizeof( int )*nsize );
	nclloc= ( int *)   malloc( sizeof( int)* nsize + 1);
	ncolid= ( int *) 	 malloc( sizeof( int )*nsize );
	colktmin = ( float *) malloc( sizeof( float  )*nsize );
	jcolktmin= ( int *) malloc( sizeof( int )*nsize );

/* check for malloc problem */
	if ( pcol == NULL || pmkl == NULL || ncol == NULL ||
		  nclloc == NULL ||
		  ncolid == NULL || colktmin == NULL || jcolktmin == NULL )
		  exit(24);

/* each column can have at most -ncolumns- entries */
	for ( i=0 ; i < nsize ; ++i )
	{
		pcol[i]	= (int *) malloc( sizeof( int )* nsize );
		pmkl[i]	= (float *) malloc ( sizeof( float)*nsize );
		if ( pcol[i] == NULL || pmkl[i] == NULL )
			exit(25);
		ncol[i]	= 0;				/* start with 0 entries each*/
		ncolid[i]= i+1;			/* cluster id */
		nclloc[i+1] = i;			/* opposite of ncolid */
		jcolktmin[i]	= 0;		/* no minimums yet */
	}

}
/***************************************************************************/
/* Free up memory */
void FNAME(ktclust_done)( void)
{
	int i;

	nlis	= 0;
	free( ncol );		/* # of column entries */
	free( ncolid);		/* column id #*/
	free( nclloc);		/* cluster location */
	free( colktmin);	/* dij min of column */
	free( jcolktmin); /* j from the dij minimum */

/* delete arrays */
	for ( i=0 ; i<nsize ; ++i )
	{
		free( pcol[i] );     /* cluster id table */
		free( pmkl[i] );		/* mkl table */
	}
	free( pcol );				/* pointer to column arrays */
	free( pmkl );				/* pointer to mkl arrays */
}
/**********************************************************/
void FNAME(ktclust_fill)( void )
{
	int i;
	for ( i=0 ; i < nsize ; ++ i )
		ktclust_fill_col( i );
	return;
}
/********************************************************/
static void ktclust_fill_col( int icol )
{
/*
	int *lkvec is a pointer to the start of the KVEC zebra
	bank. We use it to access the eta,phi values of the
	4-vectors and make our table.
	*/
	int j;
	int ipoint=(ncolid[icol]-1)*NREP_KVEC;
	int jpoint;
	float ieta, iphi;
	float jeta, jphi;
	float iet;
		float mklmin= -999;
		float mkl;
		int jmklmin=0;		/* minimum for column starts at 0 */
		int jclust;
		int iclust=ncolid[icol];

/*
	For each cluster i, look at all j>i and see if it is
	within D_SEP of i. If so, put it in the array, increment
	the counter for column which holds i and calculate its dij (mkl).
	Also keep track of the minimum for each column! */

	{
		mklmin = -999;
		jmklmin = 0;
		iclust = ncolid[icol];


		iet	= ((float *)lkvec)[ipoint + JET ];
		ieta	= ((float *)lkvec)[ipoint + JETA ];
		iphi	= ((float *)lkvec)[ipoint + JPHI ];
/*
	Calculate dii and store it
	*/
		pcol[icol][0]	= iclust;
		mkl				= iet*iet;
		pmkl[icol][0]	= mkl;
		jmklmin			= iclust;
		mklmin			= mkl;
		ncol[icol]		= 1;
/*
	Now do the rest
	*/
		for( j=0 ; j < nactive ; ++j )
		{
			jclust	= ncolid[j];
		    if ( jclust > iclust ) 
		    {
				jpoint	= jclust*NREP_KVEC - NREP_KVEC;
				if ( lkvec[jpoint + JPOINT ] > 0 )
				{
					jeta	= ((float *)lkvec)[jpoint  + JETA];
					if ( fabsf( ieta-jeta) <= D_SEP )
					{
						jphi	=  ((float *)lkvec)[jpoint + JPHI];
						if ( fabsf( iphi-jphi) <= D_SEP ||
					  	TWOPI - fabsf( iphi-jphi) <= D_SEP )
						{
							pcol[icol][ncol[icol]]	= 	jclust; /* add this entry */
							mkl	= ktmkl(iclust,jclust);		  /* calculate mkl */
							/*mkl = ktjet_mkl(&iclust, &jclust ); */
							pmkl[icol][ncol[icol]]	= mkl; /* store it */
							if ( mkl < mklmin )
							{
								mklmin	= mkl;
								jmklmin	= jclust;
							}
							++ncol[icol];     			/* increase # */
						}
					}
				}
			}
		}
		/* done with this column. Save minimum */
		colktmin[icol]	= mklmin;
		jcolktmin[icol]= jmklmin;
	}
}
/***********************************************************/
void FNAME(ktclust_do)( void )
{
/* do  clustering:
	for each vector, loop over the 4-vectors in its array ONLY,
	(this is how we think we will save time, rather than always looping
	over all of them). Do for all 4-vectors (avoid double comparing
	by only looking at entires j>i. So there is no nth column).

*/
/*	int j=0; */
	int i;
	int imin;
	int jmin;
	float mklmin= -999.;

do_again:
	i	= 0;
	imin	= 0;
	/* nactive is the number of columns we are using */
	while ( i < nactive )
	{
		/* Loop over the minimums of each column, save minimum*/
			if ( ! imin  || colktmin[i] < mklmin )
			{
				mklmin	= colktmin[i];
				imin		= ncolid[i];
			}
			++i;	/* go to next column */
	}
/*
	If no minimum, we are done! */
	if ( ! imin ) return;
/*
	We have a convention, imin is always less than jmin. Make it so
	*/
	jmin	= jcolktmin[ nclloc[imin] ];
	{
		int itemp=imin;
		int jtemp=jmin;
		imin	= ( itemp < jtemp ? itemp : jtemp );
		jmin  = ( itemp < jtemp ? jtemp : itemp );
	}
/*
	At this point, we have found  the minimum of all active
	pairs. We must store this result for SUBJETS. Merge
	the two clusters. Update the relevant columns. */

/* Store results */
	/* Store mklmin, icolktmin[imin], jcolktmin[jmin] */
	/* First index is iclust, second is jclust or -1 if this is iclust=jclust
		since the jet gets removed. Third is the dij value calculated. */
	lklis[ 3*nlis + 1 ]					= (long )imin;
	lklis[ 3*nlis + 2 ]  				= ( imin == jmin ? -1 : jmin );

	( (float *) lklis)[ 3*nlis + 3 ] = mklmin;
	--nlis;										/* we have merged another one */

/* merge the two clusters: */
	ktclust_merge_cells( imin, jmin );
/*	kt_merge_cells( &imin, &jmin ); */

/* update the column arrays */

/* Now we can toss the old column */
	{
		int i = nclloc[ jmin ];
		int *hold1 =  pcol[i]; 		/* need to hold these so we can */
		float *hold2 = pmkl[i];    /* free the memory up later */

		if ( i < nactive-1 )
		{
			nclloc[ ncolid[i] ]	= -1;	/* cluster is gone */
			nclloc[ ncolid[nactive-1] ]	= i;	/* its moving */
			ncolid[i]	= ncolid[nactive-1];
			ncol[i]	  	= ncol[nactive-1];
			pcol[i]		= pcol[nactive-1];
			pmkl[i]		= pmkl[ nactive-1 ];
			colktmin[i]	= colktmin[nactive-1];
			jcolktmin[i]	= jcolktmin[nactive-1];

			pcol[nactive-1]	= hold1;
			pmkl[nactive-1]	= hold2;
		}
		--nactive;
	}

/*
	Loop over the rest of the columns:
		1) If i or j was the minimum, find a new minimum.
		2) If i is within D_SEP, update/make new entry.
		3) If i is outside D_SEP and present, delete it.
		4) Delete j if present
		5 BUT, if i==j, then no need to test. Just get rid
			of any entries you see.
	*/
	{
		int	iclust		= imin;
		int   icol			= nclloc[iclust];
		int	jclust		= jmin;
		int col;
		float ieta			= ( (float *)lkvec)[  iclust*NREP_KVEC - NREP_KVEC + JETA];
		float iphi			= ( (float *)lkvec)[  iclust*NREP_KVEC - NREP_KVEC + JPHI];

		for ( col=0; col < nactive ; ++col )
		{
			int find_newmin	= 0;  /* do we need to find a new minimum */
			int found_newmin	= 0;  /* Did iclust happen to be a new minimum? */
			int iwithin_dsep	= 0;  /* is iclust within D_SEP of this cluster? */
			int iclust_found	= 0;  /* is iclust in this column's list? */
			int newmin			= 0;
			int isjet			= ( iclust == jclust ); /* This was a jet and is gone*/
			float mkmin= -999.;
			int ientry;

/* Skip icol for now, as we have to do the whole thing again anyhow */
			if ( col == icol ) goto skip_col;

/* Need to find a new minimum AT LEAST, if one of the minimum clusters changed */
			find_newmin	=
				( jcolktmin[col] == iclust ||
				  jcolktmin[col] == jclust   );

/* If our new iclust is now close enough to this one, we must update/add it */
/* So determine if this is TRUE */
			if ( ! isjet ) /* do this only if we merged two clusters */
				if ( fabsf( ieta - ( (float *)lkvec)[ ncolid[col]*NREP_KVEC - NREP_KVEC + JETA ] )
					<= D_SEP )
				{
					float jphi = ( (float *)lkvec)[ ncolid[col]*NREP_KVEC - NREP_KVEC + JPHI];
					jphi	= fabsf(jphi-iphi);
					if ( min( jphi, TWOPI-jphi) <= D_SEP )
						iwithin_dsep	= 1;
				}

		/* Loop over entries now */
			for ( ientry = 0 ; ientry < ncol[col] ; ++ientry )
			{
				int this_cluster = pcol[col][ientry];

				/* Look for jclust or iclust if we dont want it here */
				if ( this_cluster == jclust ||
					  ( this_cluster == iclust && ! iwithin_dsep ))
				{ /* kill it */
					pcol[col][ientry]	=	pcol[col][ncol[col]-1];
					pmkl[col][ientry]	= 	pmkl[col][ncol[col]-1];
					--(ncol[col]); /* one less entry */
					--ientry;		/* so they will do  this one next */
				}
				else
				{
					/* need to find a new minimum? */
					if ( find_newmin && this_cluster != iclust )
					{
						if ( ! newmin || pmkl[col][ientry] < mkmin )
						{
							mkmin	= pmkl[col][ientry];
							newmin	= this_cluster;
						}
					}
					else if ( this_cluster == iclust )
					{  /* recalculate it and see if it is a minimum! */
						float thismkl;

						iclust_found		 = 1;
						thismkl				 = ktmkl( ncolid[col], iclust ); /*ktjet_mkl(&(ncolid[col]), &iclust);*/
						pmkl[col][ientry]  = thismkl;
						if ( thismkl < ( ! find_newmin ? colktmin[col] :
										( newmin ? mkmin : 2*thismkl) ))
						{
							/* Oops. This guy is a new minimum. Flag that we found a
								new minimum even if we werent looking! The ones before this
								cannot matter as they couldnt be less than the old minimum.
							*/
							found_newmin	= 1;
							newmin			= iclust;
							mkmin				= thismkl;
						}
					}
				}
			}
/*
  	If this new cluster should be in this column's circle, but was not, Add it, see
  	if it is a minimum.
  */
  
			if ( ! iclust_found && iwithin_dsep )
			{
				++ncol[col];
				pcol[col][ncol[col]-1]	= iclust;
				pmkl[col][ncol[col]-1]	= ktmkl( ncolid[col], iclust ); /*ktjet_mkl( &(ncolid[col]),&iclust);*/
				if  (newmin && pmkl[col][ncol[col]-1] < mkmin )
				{
					found_newmin	= 1;
					mkmin			= pmkl[col][ncol[col]-1];
					newmin			= iclust;
				}
			}
/* Fill new minimum if needed or if one was found */
			if ( find_newmin || found_newmin )
			{
				colktmin[col]	   = mkmin;
				jcolktmin[col]		= newmin;
			}
		skip_col: 
		;				
		}
	}
/* Finally we must remake the imin column but only if this was a merge */
	if ( lklis[ 3*nlis + 3 + 2 ]  != -1 ) ktclust_fill_col( nclloc[imin]  );

	goto do_again;				/* continue */
}

/* ************************************************************************/
/* ktclust_merge_cells: merge the two, always put greater into lesser */
static void ktclust_merge_cells( int ivect, int jvect )
{
	int i,j;
	int ii,jj;
	float theta;
	float sumet, et1, et2, dphi;
	float phi1, phi2, eta1, eta2;
	i	= min( ivect, jvect );
	j	= max( ivect, jvect );
	ii	= (i-1)*NREP_KVEC;
	jj	= (j-1)*NREP_KVEC;

/* dumb way to add */
	if ( i != j )
	{
/* add the layer fractions and cells */
	((float *) lkvec)[ii+KVEC_EM] = ( ((float*) lkvec)[ii+KVEC_EM]*
		 ((float*) lkvec)[ii+JET]+((float*) lkvec)[jj+KVEC_EM]*
		 ((float*) lkvec)[jj+JET])/(((float*) lkvec)[ii+JET]+((float*) lkvec)[jj+JET]);
	((float *) lkvec)[ii+KVEC_ICD] = ( ((float*) lkvec)[ii+KVEC_ICD]*
		 ((float*) lkvec)[ii+JET]+((float*) lkvec)[jj+KVEC_ICD]*
		 ((float*) lkvec)[jj+JET])/(((float*) lkvec)[ii+JET]+((float*) lkvec)[jj+JET]);
    ((float *) lkvec)[ii+KVEC_FH] = ( ((float*) lkvec)[ii+KVEC_FH]*
		 ((float*) lkvec)[ii+JET]+((float*) lkvec)[jj+KVEC_FH]*
		 ((float*) lkvec)[jj+JET])/(((float*) lkvec)[ii+JET]+((float*) lkvec)[jj+JET]);
		                        
    ((float *) lkvec)[ii+KVEC_NCELL]=((float *) lkvec)[ii+KVEC_NCELL]+
		 ((float *) lkvec)[jj+KVEC_NCELL];
    ((float *) lkvec)[ii+KVEC_CELL_EM]=((float *) lkvec)[ii+KVEC_CELL_EM]+
		 ((float *) lkvec)[jj+KVEC_CELL_EM];
    ((float *) lkvec)[ii+KVEC_CELL_ICD]=((float *) lkvec)[ii+KVEC_CELL_ICD]+
		 ((float *) lkvec)[jj+KVEC_CELL_ICD];
    ((float *) lkvec)[ii+KVEC_CELL_FH]=((float *) lkvec)[ii+KVEC_CELL_FH]+
		 ((float *) lkvec)[jj+KVEC_CELL_FH];
    
/* Add the 4-vectors */
		((float *) lkvec)[ii+ JPX ] = ((float *) lkvec)[ii+ JPX ] +
			((float *) lkvec)[jj + JPX];
		((float *) lkvec)[ii+ JPY ] = ((float *) lkvec)[ii+ JPY ] +
			((float *) lkvec)[jj + JPY];
		((float *) lkvec)[ii+ JPZ ] = ((float *) lkvec)[ii+ JPZ ] +
			((float *) lkvec)[jj + JPZ];
		((float *) lkvec)[ii+ JE ] = ((float *) lkvec)[ii+ JE ] +
			((float *) lkvec)[jj + JE];
		
/*
  More complicated. Do the eta,phi,ET calculation
  for the different definitions
  */
		et1 =  ((float *) lkvec)[ ii + JET ];
		et2 =  ((float *) lkvec)[ jj + JET ];
		eta1 =  ((float *) lkvec)[ ii + JETA];
		eta2 =  ((float *) lkvec)[ jj + JETA ];
		phi1 =  ((float *) lkvec)[ ii + JPHI ];
		phi2 =  ((float *) lkvec)[ jj + JPHI ];
		dphi = phi2 - phi1;
		sumet	= et1 + et2;
		
/* Handle the different cases */
		if ( etadef == 1 )
			FNAME(etoeta)( ((float *) lkvec+ii+JPX), ((float *) lkvec+ii+JPHI),
					&theta, ((float *) lkvec+ii+JETA) );
		else
		{
			((float *) lkvec)[ ii + JETA]  = (eta1*et1+eta2*et2)/(sumet); /* snowmass */
			((float *) lkvec)[ ii + JPHI ] =
				phi1 + ( fabsf(dphi) <= PI ? dphi : (dphi/fabsf(dphi))*(fabsf(dphi)- TWOPI ) )*et2/(sumet);
			theta	= 2.0*atan( exp( -(((float *) lkvec)[ii+JETA])));
		}
		/* Map phi into range of 0 to TWOPI */
		if ( ((float *) lkvec)[ii+JPHI] < 0.0 ) ((float *) lkvec)[ii+JPHI] = 
			((float *) lkvec)[ii+JPHI] + TWOPI;
		if ( ((float *) lkvec)[ii+JPHI] > TWOPI ) ((float *) lkvec)[ii+JPHI] = 
			((float *) lkvec)[ii+JPHI] - TWOPI;
		
		/* Do ET */
		if ( etdef == 1 )
			((float *) lkvec)[ii+JET]	= sqrt( ((float *) lkvec)[ii+JPX]*((float *) lkvec)[ii+JPX] +
											     ((float *) lkvec)[ii+JPY]*((float *) lkvec)[ii+JPY] );
		else if ( etdef == 2 )
			((float *) lkvec)[ii+JET]  = ((float *) lkvec)[ii+JE]*sin(theta);
		else
			((float *) lkvec)[ ii + JET ]  = sumet;	 						/* sum et */
/*
  Update the map
  */
		{
			int i11 = lkvec[ ii + JPOINT];
			int i22 = lkvec[ jj + JPOINT];
			int in1 = lkmap[ NREP_KMAP*(i11-1) + 12];
			int in2 = lkmap[ NREP_KMAP*(i22-1) + 12];
		
			lkmap[ NREP_KMAP*(i11-1) + 12]	=	in2;
			lkmap[ NREP_KMAP*(i22-1) + 12]	=	in1;
		}
	
	}
/* ivalidate the j cluster */
	lkvec[jj + JPOINT ]	= -(lkvec[jj+JPOINT]);
}

/*****************************************************************************/
/*
	ktmkl: defines the kt metric between clusters i and j
	*/
static float ktmkl( int ivect, int jvect )
{
	float et1, et2, eta2, eta1, phi1, phi2;
	int ipoint, jpoint;

	ipoint	= (ivect-1)*NREP_KVEC;
	jpoint	= (jvect-1)*NREP_KVEC;
	et1	=	( (float *) lkvec)[ ipoint + JET ];

	if ( ivect == jvect ) return (et1*et1);	/* dii = et**2 */

	et2	=	( (float *) lkvec)[ jpoint + JET ];
	eta1	=	( (float *) lkvec)[ ipoint + JETA ];
	eta2	=	( (float *) lkvec)[ jpoint + JETA ];
	phi1	=	( (float *) lkvec)[ ipoint + JPHI ];
	phi2	=	( (float *) lkvec)[ jpoint + JPHI ];

	if ( fabsf(phi1-phi2) <= PI )
		return( min(et1*et1,et2*et2)*((eta1-eta2)*(eta1-eta2)+(phi1-phi2)*
			(phi1-phi2)) );

	phi1	= TWOPI - fabsf(phi1-phi2);
	return( min(et1*et1,et2*et2)*( (eta1-eta2)*(eta1-eta2)+phi1*phi1 ) );
}


