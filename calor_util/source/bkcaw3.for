      SUBROUTINE BKCAW3(LCASH,NCELLS,LCAW3)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the Bank CAW3
C-
C-   Inputs  : LCASH = Link of parent bank
C-
C-       CAW3 hangs from CASH.  It contains the cells in a window 3x3
C-       around the hottest tower in CASH.
C-
C-   Outputs : LCAW3  [I] -   Link of Booked CAW3 Bank
C-   Controls: None
C-
C-   Updated  24-JUL-1995   R. J. Genik II Modify BKCAW7 to book CAW3
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCAW3.LINK'
C
      INTEGER LCAW3,LCASH
      INTEGER IXIO
      INTEGER NCELLS
      INTEGER NUM_STRUC_LINKS
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      LCAW3 = 0
C
      IF(FIRST)THEN
        FIRST = .FALSE.
        CALL MZFORM('CAW3','2I/1I1F',IXIO)        ! Describe Bank format
      ENDIF
C
      IF (LCASH.LE.0) THEN
        CALL ERRMSG('NO CASH','BKCAW3',' ','W')
        GOTO 999
      ENDIF
C
      NUM_STRUC_LINKS = IQ(LCASH-2)
      IF (NUM_STRUC_LINKS.LT.IZCAW3) THEN
        CALL ERRMSG('NOT ENOUGH STR. LINKS','BKCAW3','Book CAW7 First',
     +    'W')
        GOTO 999
      ENDIF
C
      CALL MZBOOK
     &  (IXMAIN,LCAW3,LCASH,-IZCAW3,'CAW3',0,0,2*NCELLS+2,IXIO,0)
C
      IQ(LCAW3+1) = 1
      IQ(LCAW3+2) = NCELLS
C
  999 RETURN
      END
