      SUBROUTINE CHECK_MU_QUALITY(LPMUO,MUMASK,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check the Bit Pattern in Word 44(42) of
C-                         PMUO to see if this is a good muon
C-                                  track candidate.
C-
C-   Inputs  : 
C-              LPMUO     (I) - PMUO Bank pointer   
C-              MUMASK    (I) - 32 bit integer mask for active cuts
C-       
C-   Outputs : 
C-              OK        (L) - .TRUE. If muon satisfies MUON_MASK 
C-   Controls: 
C-              None
C-
C-   Created  28-Jun-1993   Stephen J. Wimpenny
C-   Modified 29-Sep-1993   Darien Wood, use single integer mask as
C-                          in CHECK_EM_QUALITY.  MU_SET_QUAL_MASK
C-                          now handles arrays
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      LOGICAL OK,FIRST
C
      INTEGER I,LPMUO,STATUS,MUMASK,IER
C
      DATA FIRST/.TRUE./
C
C
      IF(LPMUO.GT.0) THEN
C
C *** Check on Bank version
C
        IF(IQ(LPMUO+1).GE.3) THEN
C
C *** Versions 3 and higher
C
         STATUS=IQ(LPMUO+45)
        ELSE
C
C *** Versions 1 and 2
C
         STATUS=IQ(LPMUO+42) 
       ENDIF
       OK=.TRUE.
       IF(IAND(STATUS,MUMASK).NE.0) THEN
         OK=.FALSE.
       ENDIF
      ELSE
        OK=.FALSE.
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
