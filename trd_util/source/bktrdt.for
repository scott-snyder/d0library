      SUBROUTINE BKTRDT(LTRDT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the Bank TRDT
C-
C-   Inputs  : None
C-   Outputs : Link of Booked TRDT Bank
C-   Controls: None
C-
C-   Created  20-OCT-1989 18:36:53.08  A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LTRDT
      INTEGER LTTRH,GZTTRH
      INTEGER IXIO
C
C--   ZEBRA BANKS
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZTRDT.LINK/LIST'
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
C--   INITIALIZE
C
      LTRDT = 0
      IF(FIRST)THEN
C
        CALL MZFORM('TRDT','1F 2I 19F 1I  -F',IXIO)   ! Describe
C                                                     ! Bank format
        FIRST=.FALSE.
C
      ENDIF
C
C--   FIND LINK TO SUPPORTING PARENT BANK
      LTTRH=GZTTRH()
      IF(LTTRH.LE.0)THEN
        CALL BKTTRH(LTTRH)
        IF(LTTRH.LE.0)THEN
          CALL ERRMSG(' Problem booking TTRH','bktrdt',' ','W')
          go to 999
        END IF
        Q(LTTRH+1)=0.0
      END IF
C
C
      CALL MZBOOK
     &  (IXMAIN,LTRDT,LTTRH,-IZTRDT,'TRDT',5,3,42,IXIO,0)
C
  999 RETURN
      END
