      SUBROUTINE BKCAWC(LCASH,NCELLS,LCAWC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the Bank CAWC
C-
C-   Inputs  : LCASH = Link of parent bank
C-
C-       CAWC hangs from CASH.  It contains the cells in a window 3x3
C-       around the hottest tower in CASH.
C-
C-   Outputs : LCAWC  [I] -   Link of Booked CAWC Bank
C-   Controls: None
C-
C-   Updated  24-JUL-1995   R. J. Genik II Modify BKCAW7 to book CAWC
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCAWC.LINK'
C
      INTEGER LCAWC,LCASH
      INTEGER IXIO
      INTEGER NCELLS
      INTEGER NUM_STRUC_LINKS
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      LCAWC = 0
C
      IF(FIRST)THEN
        FIRST = .FALSE.
        CALL MZFORM('CAWC','2I/1I1F',IXIO)        ! Describe Bank format
      ENDIF
C
      IF (LCASH.LE.0) THEN
        CALL ERRMSG('NO CASH','BKCAWC',' ','W')
        GOTO 999
      ENDIF
C
      NUM_STRUC_LINKS = IQ(LCASH-2)
      IF (NUM_STRUC_LINKS.LT.IZCAWC) THEN
        CALL ERRMSG('NOT ENOUGH STR. LINKS','BKCAWC','Book CAW7 First',
     +    'W')
        GOTO 999
      ENDIF
C
      CALL MZBOOK
     &  (IXMAIN,LCAWC,LCASH,-IZCAWC,'CAWC',0,0,2*NCELLS+2,IXIO,0)
C
      IQ(LCAWC+1) = 1
      IQ(LCAWC+2) = NCELLS
C
  999 RETURN
      END
