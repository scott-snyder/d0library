      SUBROUTINE VTMW_FROM_ASCII(OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill VTMW bank from T0s stored in an ASCII file
C-
C-   Inputs  : FILENAME of ASCII file
C-   Outputs : OK if all ok
C-   Controls: 
C-
C-   Created   2-AUG-1992   Ed Oltman
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER IUSER,LUN,IER
      INTEGER LAYER,LVTMW(0:2),LABEL,TYPE,SECTOR,WIRE,STRIP,WEND,UBIT
      INTEGER ITEMS,J,LENGTH
      CHARACTER*60 T0_FILE
      REAL T0,T0ERR
      LOGICAL OPENED,OK
      INTEGER GZVTMW
      DATA IUSER/731/                 ! This is a random number between 100 and
                                      ! 1000 (see GTUNIT.FOR)
C----------------------------------------------------------------------
      CALL EZPICK('VTWSTP_RCP')
      CALL EZGETS('T0_FILE',1,T0_FILE,LENGTH,IER)
      CALL EZRSET
      OK = .FALSE.
      CALL GTUNIT(IUSER,LUN,IER)
      CALL D0OPEN(LUN,T0_FILE,'IF',OPENED)
      IF (.NOT. OPENED) THEN
        CALL ERRMSG('Error opening file','VTMW_FROM_ASCII',
     &         't0-file not found','W')
        CALL RLUNIT(IUSER,LUN,IER)
        GO TO 999
      ENDIF
      DO LAYER = 0,2
        LVTMW(LAYER) = GZVTMW(LAYER)
      ENDDO
      ITEMS = IC(LVTMW(0)+3)
    1 READ(LUN,*,END=2) LABEL,T0,T0ERR
      CALL VCODER(LABEL,TYPE,LAYER,SECTOR,WIRE,STRIP,WEND,UBIT,1)
      J = LVTMW(LAYER) + (SECTOR*8 + WIRE)*ITEMS + 5
      C(J+2*WEND+1) = T0
      C(J+2*WEND+2) = T0ERR
      C(J+       5) = 1.
      GOTO 1
    2 OK = .TRUE.
      CALL RLUNIT(IUSER,LUN,IER)
  999 RETURN
      END
