      SUBROUTINE BKTHIT(LTHIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the Bank THIT
C-
C-   Inputs  : LTRDH = Link of parent bank.
C-                      = 0, will find it for you.
C-   Outputs : Link of Booked THIT Bank
C-   Controls: None
C-
C-   Created  22-JUL-1991 16:29:49.47  A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LTHIT
      INTEGER LTRDH
      INTEGER IXIO
      INTEGER GZTRDH
C
C--   ZEBRA BANKS
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZTHIT.LINK/LIST'
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
C--   INITIALIZE
C
      IF(FIRST)THEN
        FIRST=.FALSE.
        CALL MZFORM('THIT',' -I',IXIO)        ! Describe Bank format
      END IF
C
C--   FIND LINK TO SUPPORTING PARENT BANK
C
      LTHIT = 0
      LTRDH = GZTRDH()
      IF(LTRDH.LE.0)CALL BKTRDH(LTRDH)
      IF(LTRDH.LE.0 )THEN
        CALL ERRMSG('Cant find bank TRDH ','in BKTHIT',' ','W')
        GO TO 999
      END IF
      CALL MZBOOK
     &  (IXMAIN,LTHIT,LTRDH,-IZTHIT,'THIT',0,0,10600,IXIO,0)
C
  999 RETURN
      END
