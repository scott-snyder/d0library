      SUBROUTINE TOP_LEPTONS_LONG_JET_DUMP(LUN,LJETS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Long Jet Dump 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  22-APR-1993   Stephen J. Wimpenny
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      INTEGER LUN,ITEMP,JTEMP,LJETS
C
C *** for splitting flag -> truncate word to get rid of selection
C *** flag bits (bits 17-32) of LJETS word 15
C
      JTEMP=IQ(LJETS+15)/100
      ITEMP=IQ(LJETS+15)-100*JTEMP
      WRITE(LUN,1010) Q(LJETS+12),Q(LJETS+13),IQ(LJETS+16),ITEMP,
     1  Q(LJETS+14),Q(LJETS+17),Q(LJETS+18),Q(LJETS+19)
C
C----------------------------------------------------------------------
  999 RETURN
 1010 FORMAT(5X,' Rms widths in eta , phi = ',2F8.2,
     1 ' No cells above 1 GeV = ',I4,/, 
     2 5X,' Merge/splitting flag = ',I3,/,
     3 5X,' em Et Fraction , ICD+MG Fraction , CH Fraction = ',
     4 3F8.2,/,5X' Ratio hottest to next-hottest cell = ',F8.2)
      END
