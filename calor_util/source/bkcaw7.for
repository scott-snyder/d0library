      SUBROUTINE BKCAW7(LCASH,NCELLS,LCAW7)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the Bank CAW7
C-
C-   Inputs  : LCASH = Link of parent bank
C-
C-       CAW7 hangs from CASH.  It contains the cells in a window
C-       around the hottest tower in CASH.  CAWX is made from CAW7
C-       and contains those cells in CAW7 which are not in CASH.
C-
C-   Outputs : LCAW7  [I] -   Link of Booked CAW7 Bank
C-   Controls: None
C-
C-   Updated  24-JUL-1995   Ian Adam   Modify BKCASH to do CAW7
C-   Updated  23-AUG-1995   R. J. Genik II   Add another link to make 5
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCAW7.LINK'
      INTEGER CASH_VERSION
      PARAMETER (CASH_VERSION = 2)
C
      INTEGER LCAW7,LCASH
      INTEGER IXIO
      INTEGER NCELLS
      INTEGER NUM_STRUC_LINKS,NUM_EXTRA_LINKS
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      LCAW7 = 0
C
      IF(FIRST)THEN
        FIRST = .FALSE.
        CALL MZFORM('CAW7','2I/1I1F',IXIO)        ! Describe Bank format
      ENDIF
C
      IF (LCASH.LE.0) THEN
        CALL ERRMSG('NO CASH','BKCAW7',' ','W')
        GOTO 999
      ENDIF
C
      NUM_STRUC_LINKS = IQ(LCASH-2)
      IF (NUM_STRUC_LINKS.LT.IZCAW7) THEN
        NUM_EXTRA_LINKS = 5
        CALL MZPUSH(IXCOM,LCASH,NUM_EXTRA_LINKS,0,' ')
C change CASH bank version number
        IQ(LCASH + 1) = CASH_VERSION
      ENDIF
C
      CALL MZBOOK
     &  (IXMAIN,LCAW7,LCASH,-IZCAW7,'CAW7',0,0,2*NCELLS+2,IXIO,0)
C
      IQ(LCAW7+1) = 1
      IQ(LCAW7+2) = NCELLS
C
  999 RETURN
      END
