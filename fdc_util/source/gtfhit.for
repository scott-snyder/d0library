      SUBROUTINE GTFHIT(IHIT,QFHIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns FHIT words for a single compressed
C-      hit. (use FHIT_DECODE to decode these two words).
C-
C-   Inputs  : IHIT     Hit number
C-   Outputs : QFHIT(2)  Compressed hit.
C-      If IHIT = 0, return QFHIT(1) = number of hits (INTEGER)
C-
C-   Created  10-OCT-1991   Robert E. Avery
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C INPUT:
      INTEGER IHIT
C OUTPUT:
      REAL    QFHIT(*)
C LOCAL:
      INTEGER LFHIT,GZFHIT,LHIT 
C----------------------------------------------------------------------
      LFHIT = GZFHIT()
      IF ( LFHIT.GT.5 ) THEN
        IF ( IHIT.GT.0 ) THEN
          LHIT = LFHIT + 4 + (IHIT-1)*IQ(LFHIT+3)
          CALL UCOPY(Q(LHIT),QFHIT,2)
        ELSE
          CALL UCOPY(Q(LFHIT+2),QFHIT,1)
        ENDIF
      ELSE
        CALL VFILL(QFHIT,1,-1)
      ENDIF
  999 RETURN
      END
