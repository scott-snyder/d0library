      SUBROUTINE GTPLV0_ZONLY(FZPOS,FGOOD,SZPOS,SGOOD,MI_FLAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get the L0 vertex data words from the PLV0 bank.
C-
C-   Inputs  : none
C-   Outputs : FZPOS   [F] : FASTZ vertex position (cm)
C-             FGOOD   [L] : TRUE if FASTZ vertex is good
C-             SZPOS   [F] : Slow Z vertex position (cm)
C-             SGOOD   [L] : TRUE if Slow Z vertex good
C-             MI_FLAG [I] : Multiple Interaction flag value (1-4)
C-   Controls: none
C-
C-   Created  15-DEC-1992   Jeffrey Bantly
C-   Updated  21-JUN-1993   Jeffrey Bantly  remove Interaction bit requirement
C-                   because older versions of DSTs do not have the bit stored
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C
      INTEGER MI_FLAG
      INTEGER LKPLV0
      INTEGER GZPLV0
      EXTERNAL GZPLV0
      REAL    FZPOS, SZPOS
      LOGICAL FGOOD, SGOOD
C-----------------------------------------------------------------
      LKPLV0=GZPLV0()
      IF ( LKPLV0.GT.0 ) THEN
        FZPOS = Q(LKPLV0+2)
        SZPOS = Q(LKPLV0+3)
        FGOOD = .FALSE.
        IF ( IBITS(IQ(LKPLV0+1),0,1).EQ.1 ) FGOOD = .TRUE.
        SGOOD = .FALSE.
        MI_FLAG = 0
c        IF ( IBITS(IQ(LKPLV0+1),12,1).EQ.1 ) THEN
          IF ( IBITS(IQ(LKPLV0+1),5,1).EQ.1 ) SGOOD = .TRUE.
          IF ( SGOOD ) THEN
            IF ( IBITS(IQ(LKPLV0+1),3,1).EQ.1 ) MI_FLAG=3
            IF ( IBITS(IQ(LKPLV0+1),4,1).EQ.1 ) MI_FLAG=4
            IF ( IBITS(IQ(LKPLV0+1),2,1).EQ.1 ) MI_FLAG=2
            IF ( IBITS(IQ(LKPLV0+1),1,1).EQ.1 ) MI_FLAG=1
          ENDIF
c        ENDIF
      ELSE
        FZPOS = -9.0
        FGOOD = .FALSE.
        SZPOS = -9.0
        SGOOD = .FALSE.
        MI_FLAG = 0
      ENDIF
C-----------------------------------------------------------------
  999 RETURN
      END
