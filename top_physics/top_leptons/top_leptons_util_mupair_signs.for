      SUBROUTINE TOP_LEPTONS_UTIL_MUPAIR_SIGNS(LPMUO1,LPMUO2,SIGN1,
     1  SIGN2,PAIR,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the signs of the two leading
C-                         muons in the event
C-
C-   Inputs  : 
C-              LMPUO1 - Pointer to 1st PMUO Bank
C-              LPMUO2 - Pointer to 2nd PMUO Bank
C-
C-   Outputs : 
C-              SIGN1 - charge of leading muon
C-              SIGN2 - charge of second moun
C-              PAIR  - nett charge of pair
C-              IER = 1/-1 for OK/Bad return
C-
C-   Controls: 
C-
C-   Created   1-OCT-1992   Stephen J. Wimpenny
C-   Modified 22-Mar-1993   Routine name changed for library compatibility
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      INTEGER LPMUO1,LPMUO2,SIGN1,SIGN2,PAIR,IER
C
      IER=-1
      IF(LPMUO1.GT.0.AND.LPMUO2.GT.0) THEN
        SIGN1=1
        IF(IQ(LPMUO1+2).GT.0) SIGN1=-1
        SIGN2=1
        IF(IQ(LPMUO2+2).GT.0) SIGN2=-1
        PAIR=SIGN1+SIGN2
        IER=1
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
