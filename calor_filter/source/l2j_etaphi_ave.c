/*
  l2j_etaphi_ave( int *ie_start, int *ip_start, int *ie_off_low, 
	 int *ip_off_low, int *ie_off_high, int *ip_off_high,
	 unsigned char *p_fadc_block, int *ie_ave, int *ip_ave, float *em_et
	 , float *total_et )
  
  l2j_eta_phi_ave: Look in an area around a L1 seed tower and return the
  				   summed EM and TOTAL ET's as well as the L1 tower which
  				   contains the total ET weighted eta-phi average.
  				   Note we use the convention of L1 phi from 1:32 and
  				   L1 eta from 1:40 (1:20 maps to -20:-1 and 21:40 maps to
  				   1:20.
  
  Inputs : ie_start, ip_start - Pointers to initial L1 tower. ip_start
   							    should be between 1-32, ie_start between 1-40
           ie_off_low		  - Pointers to offset from initial tower where
  		   ip_off_low           we should start 
  		   ie_off_high			* example * - to do the nearest neighbors 
  	       ip_off_high		    you would set ie_off_low=ip_off_low=-1
  		 						and ie_off_high=ip_off_high = +1
  			p_fadc_block	  - Pointer to start of L1 FADC block
  Outputs:
  			ie_ave, ip_ave    - Pointers to ET weighted average L1 tower
  			em_et, total_et   - Pointers to summed ET's over this area.

         Created           : 22-AUG-1993 by Richard V. Astur
  		 updated 18-Dec-95 by D Owen. fixed energy weighted possition calc
  		 updated  5-Jan-96 by N Varelas. protect division by zero energy
-------------------------------------------------------------------------
*/
 
#include <stdio.h>                   /* I/O definitions                       */
#include <math.h>
#include "d0$params:l1_params.h"	 /* l1 cal parameters */
#include "d0$inc:l2j_etaphi_util.h"	 /* Lookup arrays to accesss L1C FADC's */

void l2j_etaphi_util_init();		 /* initialization routine for lo arrays*/

l2j_etaphi_ave( int *ie_start, int *ip_start, int *ie_off_low, 
	 int *ip_off_low, int *ie_off_high, int *ip_off_high,
	 unsigned char *p_fadc_block, int *ie_ave, int *ip_ave, float *em_et
	 , float *total_et )
{
	static Imod  = 2;					/* control truncation of trigger */
	int ieta_del = 0;					/* tower ET (1=none 2=1/4 drop ) */
	int iphi_del = 0;
	int ietot    = 0;
	int ietot_untrnc = 0;
	int iemtot   = 0;
	int iettow;							/* et of trigger tower (total) */
	int ii, jj, j, i;
	unsigned char iemdel;
	unsigned char ihadel;
	int iedel;		

/* init offset arrays 
   */
	l2j_etaphi_util_init();

/*
  	Loop over FADC block values.  Pointer currently points to first byte
  of block, so decrement it to point before
  */
	--p_fadc_block;
	
	for ( ii = *ie_start + *ie_off_low + L15BUF - 1;
		  ii <= *ie_start + *ie_off_high + L15BUF - 1 ; ++ ii )
	{
		
		for ( jj = *ip_start + *ip_off_low + L15BUF - 1 ; 
			  jj <= *ip_start + *ip_off_high + L15BUF - 1 ; ++ jj )
		{
			iemdel = *( p_fadc_block + l1_phi_off[jj] +
				l1_eta_off[ii] ) ;
			ihadel = *( p_fadc_block + l1_phi_off[jj] +
				l1_eta_off[ii] + l1_eta_had_off[ii] );
			iettow = Imod*(( iemdel + ihadel )/Imod);
			ietot = ietot + iettow - 16;			/* subtract 8 counts */
			ietot_untrnc = ietot_untrnc + iemdel + ihadel - 16; /*untrunc sum */
			iemtot= iemtot + iemdel - 8;				/* offset from both */
			iedel = iemdel + ihadel - 16;				/* em and hadronic */
			ieta_del = ieta_del + iedel * ( ii - (*ie_start + L15BUF - 1));
			iphi_del = iphi_del + iedel * ( jj - (*ip_start + L15BUF - 1));
		}
	}
	ietot = ( ietot < 1 ? 1 : ietot );			      /* divide by 0 protect*/
	ietot_untrnc = ( ietot_untrnc < 1 ? 1 : ietot_untrnc );	/* divide by 0 protect*/
	*ie_ave = ( (int) ( ( ieta_del > 0 ? .5 : -.5 ) + /* find new center */ 
		( (float) ieta_del)/( (float) ietot_untrnc ) )) + *ie_start;
	*ie_ave = ( *ie_ave > 2*NL1ETAL ? 2*NL1ETAL 
		: ( *ie_ave < 1 ? 1 : *ie_ave ));
	*ip_ave = ( (int) (( iphi_del > 0 ? .5 : -.5 ) + 
		( (float) iphi_del)/( (float) ietot_untrnc ) )) + *ip_start;
	*ip_ave = ( *ip_ave > NL1PHIL ? NL1PHIL 
		: ( *ip_ave < 1 ? 1 : *ip_ave ));
	*em_et   = iemtot/4.;
	*total_et= ietot/4.;
}
