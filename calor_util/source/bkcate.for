      SUBROUTINE BKCATE(NCHT,LCATE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-          book CATE, calorimeter towers
C-
C-   Inputs  : 
C-     NCHT  = number of channels
C-   Outputs : 
C-     LCATE = pointer to CATE
C-
C-   Created  26-APR-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NCHT,LCATE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCATE.LINK'
      INTEGER LCAHT,GZCAHT,NALOC,NRT,IOH
      PARAMETER (NRT=14)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        CALL MZFORM('CATE','3I/7F7I',IOH)
        FIRST=.FALSE.
      ENDIF
      LCAHT=GZCAHT()
      NALOC=NCHT*NRT+3
      CALL MZBOOK(IXMAIN,LCATE,LCAHT,-IZCATE,
     $                    'CATE',1,1,NALOC,IOH,0) 
      IQ(LCATE+1)=1         ! Bank version 
      IQ(LCATE+2)=NRT       ! repetition no.
  999 RETURN
      END
