      SUBROUTINE L2_ZCDCFL(ZVERTX,ZERROR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : book and fill VERT bank in Level 2
C-
C-   Inputs  : ZVERTX: vertex's Z position
C-             ZERROR: error on vertex's Z position
C-   Outputs : none
C-
C-   Created  2-AUG-1992   Qizhong Li-Demarteau
C-   Updated  21-DEC-1993   Qizhong Li-Demarteau  removed filling HSTR
C-                                         (it is filled in BKVERH now)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      REAL    ZVERTX, ZERROR
      INTEGER LVERH, LVERT, GZVERH, NR
      DATA NR /10/
C----------------------------------------------------------------------
C
C  book and fill VERH bank, if VERH bank does not exist
C
      LVERH = GZVERH()
      IF (LVERH.LE.0) THEN
        CALL BKVERH(LVERH)
        IQ(LVERH+1) = 0
        IQ(LVERH+2) = 0
      ENDIF
C
C  book and fill VERT bank
C
      CALL BKVERT(LVERT,NR)
      IF (LVERT .LE. 0) GOTO 999
      LVERH = GZVERH()
      IQ(LVERH + 2) = IQ(LVERH + 2) + 1
      IQ(LVERT+1) = 0
      Q(LVERT+5) = ZVERTX
      Q(LVERT+8) = ZERROR
C
  999 RETURN
      END
