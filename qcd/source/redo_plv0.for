      SUBROUTINE REDO_PLV0
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :   Remake PLV0 bank from TRGR
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  20-AUG-1993   Brent J. May
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LPLV0, GZPLV0
      LOGICAL FIRST, LV0INI, LV0EVT, LV0DO      
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C Do LEVEL0 package
C
      IF (FIRST) THEN
        LV0DO = LV0INI() 
        FIRST = .FALSE.
      ENDIF
      LV0DO = LV0EVT()
C
C Remake PLV0 with new info
C 
      LPLV0=GZPLV0()
      IF ( LPLV0.GT.0 ) THEN
        CALL MZDROP(IXCOM,LPLV0,' ')
      ENDIF
      CALL BKPLV0(LPLV0) 
      CALL FILL_PLV0()
C
  999 RETURN
      END
