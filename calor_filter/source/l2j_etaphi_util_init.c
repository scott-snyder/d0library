/* 
   void l2j_etaphi_util_init()
   
   l2j_etaphi_util_init : Initialize the static lookup arrays which we
   						  are using to access the FADC ET's in the Level 
   						  one calorimeter trigger block. Note we often
   						  map L1 eta towers from -20:20 to 1:40 for 
   						  convenience.
   
   Inputs : none
   Ouputs : none
   Controls: Set initialization flag to TRUE when done
   
         Created           : 13-OCT-1993 by Richard V. Astur
---------------------------------------------------------------------------
   */
 
#include <stdio.h>                   /* I/O definitions                       */
#include "d0$params:l1_params.h"	 /* l1 cal parameters */

		/* #include "d0$inc:l2j_etaphi_util.h"	*/
		/* Lookup arrays to accesss L1C FADC's */
int l1_phi_off[ NL1PHIL + 2*L15BUF ];	/* FADC block offset arrays */
int l1_eta_off[ 2*NL1ETAL + 2*L15BUF ];     /* For EM       */
int l1_eta_had_off[ 2*NL1ETAL + 2*L15BUF];  /* For HADRONIC */
int fadc_off_block_init=0;	         /* Array initialization done ? */
 
void l2j_etaphi_util_init()
{
	int j, i;						/* looping variables */
	int off_eta, off_phi;
	
	if ( ! fadc_off_block_init )
	{
		fadc_off_block_init = 1;	/* init done, dont do again */
	
		for ( j=1-L15BUF; j<=NL1PHIL+L15BUF; ++j )  /* phi pointers */
		{
			off_phi = ((j - 1)+32) % 32 + 1;		/* iphi from 1-32 */
			off_phi = ( off_phi <= 16 ? 			/* pointer offset */
				2*off_phi -1 : 2*(off_phi - 16 ));
			l1_phi_off[ j + L15BUF - 1] = off_phi;  /* set offset array */
		}

		for ( i=1-L15BUF; i <= 2*NL1ETAL + L15BUF; ++i ) /* eta pointers */
		{
			if ( i < 1 || i > 2*NL1ETAL )
			{
				l1_eta_off[ i + L15BUF - 1] = NULL_FADC_OFF;
				l1_eta_had_off[ i + L15BUF - 1] = 0;
			}
			else
			{
				l1_eta_off[ i + L15BUF - 1] = ( i <= NL1ETAL 
				  ? (NL1ETAL - i)*NL1PHIL + NL1PHIL*NL1ETAL
				  : (i - NL1ETAL - 1)*NL1PHIL );
				l1_eta_had_off[ i + L15BUF - 1 ] = NL1ETAL*NL1PHIL*2;
			}
		}
	}
}
