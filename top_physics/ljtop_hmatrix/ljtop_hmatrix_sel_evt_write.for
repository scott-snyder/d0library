      SUBROUTINE LJTOP_HMATRIX_SEL_EVT_WRITE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : OVERRIDE OUTPUT SELCTION BY SWITCHES FROM RCP
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   1-OCT-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:EVENT_QUAN.INC'
      LOGICAL first
      SAVE first
      DATA first / .true. /
      INTEGER IER
      LOGICAL SEL_EVT_WRITE
      INTEGER  NEVENT1,NEVENT2,NEVENT3
      SAVE NEVENT1,NEVENT2,NEVENT3
      INTEGER JETWRITE(6)
C----------------------------------------------------------------------
      IF( first ) THEN
        FIRST = .FALSE.
        CALL EZPICK('LJTOP_HMATRIX_RCP')
        CALL EZGET('SEL_EVT_WRITE',SEL_EVT_WRITE,IER)
        CALL EZGET('JETWRITE',JETWRITE,IER)
        CALL EZRSET
        NEVENT1 = 0
        NEVENT2 = 0
        NEVENT3 = 0
      ENDIF
C
      IF ( .NOT.SEL_EVT_WRITE ) RETURN
      CALL FLGSET('WRITE_THIS_EVENT',.FALSE.)  !TURN OFF ANYWAY
C
      IF (NJETS.EQ.JETWRITE(1)  ) THEN
        NEVENT1 = NEVENT1 + 1
        IF ( NEVENT1.LE.JETWRITE(2) ) THEN
          CALL FLGSET('WRITE_THIS_EVENT',.TRUE.)
        ENDIF
      ENDIF
C
      IF (NJETS.EQ.JETWRITE(3)  ) THEN
        NEVENT2 = NEVENT2 + 1
        IF ( NEVENT2.LE.JETWRITE(4) ) THEN
          CALL FLGSET('WRITE_THIS_EVENT',.TRUE.)
        ENDIF
      ENDIF
C
      IF (NJETS.EQ.JETWRITE(5)  ) THEN
        NEVENT3 = NEVENT3 + 1
        IF ( NEVENT3.LE.JETWRITE(6) ) THEN
          CALL FLGSET('WRITE_THIS_EVENT',.TRUE.)
        ENDIF
      ENDIF
C
  999 RETURN
      END
