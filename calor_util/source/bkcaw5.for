      SUBROUTINE BKCAW5(LCASH,NCELLS,LCAW5)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the Bank CAW5
C-
C-   Inputs  : LCASH = Link of parent bank
C-
C-       CAW5 hangs from CASH.  It contains the cells in a window 5x5
C-       around the hottest tower in CASH.
C-
C-   Outputs : LCAW5  [I] -   Link of Booked CAW5 Bank
C-   Controls: None
C-
C-   Updated  24-JUL-1995   R. J. Genik II Modify BKCAW7 to book CAW5
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCAW5.LINK'
C
      INTEGER LCAW5,LCASH
      INTEGER IXIO
      INTEGER NCELLS
      INTEGER NUM_STRUC_LINKS
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      LCAW5 = 0
C
      IF(FIRST)THEN
        FIRST = .FALSE.
        CALL MZFORM('CAW5','2I/1I1F',IXIO)        ! Describe Bank format
      ENDIF
C
      IF (LCASH.LE.0) THEN
        CALL ERRMSG('NO CASH','BKCAW5',' ','W')
        GOTO 999
      ENDIF
C
      NUM_STRUC_LINKS = IQ(LCASH-2)
      IF (NUM_STRUC_LINKS.LT.IZCAW5) THEN
        CALL ERRMSG('NOT ENOUGH STR. LINKS','BKCAW5','Book CAW7 First',
     +    'W')
        GOTO 999
      ENDIF
C
      CALL MZBOOK
     &  (IXMAIN,LCAW5,LCASH,-IZCAW5,'CAW5',0,0,2*NCELLS+2,IXIO,0)
C
      IQ(LCAW5+1) = 1
      IQ(LCAW5+2) = NCELLS
C
  999 RETURN
      END
