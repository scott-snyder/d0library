/* 
       L2J_ETAPHI_UTIL.H
   
   Utility lookup arrays to facilitate moving within the L1 Cal
   FADC block
   
         Created           : 13-OCT-1993 by Richard V. Astur
*/

extern int l1_phi_off[ NL1PHIL + 2*L15BUF ];	/* FADC block offset arrays */
extern int l1_eta_off[ 2*NL1ETAL + 2*L15BUF ];     /* For EM       */
extern int l1_eta_had_off[ 2*NL1ETAL + 2*L15BUF];  /* For HADRONIC */
extern int fadc_off_block_init;	         /* Array initialization done ? */

