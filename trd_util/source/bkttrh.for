      SUBROUTINE BKTTRH(LTTRH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the Bank TTRH
C-
C-   Inputs  : None
C-   Outputs : Link of Booked TTRH Bank
C-   Controls: None
C-
C-   Created  27-OCT-1989 18:42:05.09  A. Zylberstejn
C-   Updated  23-NOV-1989   J.Fr. Glicenstein  Cleaning + debugging unit
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LTTRH,GZZTRH
      INTEGER LZZTRH
      INTEGER IXIO
C
C--   ZEBRA BANKS
      INCLUDE 'D0$INC:TRDBGU.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZTTRH.LINK/LIST'
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
C--   INITIALIZE
C
      LTTRH = 0
      IF(FIRST)THEN
C
        CALL MZFORM('TTRH','1F -I',IXIO)        ! Describe Bank format
C
        FIRST=.FALSE.
      ENDIF
C
C--   FIND LINK TO SUPPORTING PARENT BANK
C
      LZZTRH=GZZTRH()
      IF(LZZTRH.LE.0)        CALL BKZTRH(LZZTRH)
      CALL MZBOOK
     &  (IXMAIN,LTTRH,LZZTRH,-IZTTRH,'TTRH',2,2,10,IXIO,0)
  999 RETURN
      END
