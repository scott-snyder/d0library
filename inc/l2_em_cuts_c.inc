C------------------------------------------------------------------
C   Cuts for L2_EM which come as character variables
C      Oct. 5, 89                        Yi    Xia
C-   Updated  16-DEC-1991   James T. Linnemann  separate cuts from STP
C------------------------------------------------------------------
C
C...the following are fed to L2_EM as cuts.  The arrays are the full set; the
C...single values are the cuts for this call.  These are all character variables
      CHARACTER*24 CD_TR_C(NSETS),SHAPE_C(NSETS)
      COMMON/L2_EM_CUTS_C/CD_TR_C,SHAPE_C
