      SUBROUTINE BKTDST(LTDST)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the bank TDST.
C-
C-   Inputs  :
C-   Outputs : LTDST  [I] Address of booked TDST bank.
C-   Controls: None
C-
C-   Created  29-JUN-1994   A. Zylberstejn
C-   Updated  26-FEB-1996   A. Zylberstejn  :add ten more words 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LTTRH
      INTEGER GZTDST,LTDST,TDST
C----------------------------------------------------------------------
      INTEGER ND,NL,NS,IXIO
      INTEGER XND,XNL,XNS,LZFIDH
      INTEGER GZTTRH,LZTTRH
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZTDST.LINK'
C----------------------------------------------------------------------
      LOGICAL FIRST
      SAVE FIRST, IXIO
      DATA FIRST/.TRUE./
      DATA XNL,XNS,XND/3,3,35/
C----------------------------------------------------------------------
C
C--   INITIALIZE
C

      LTDST=GZTDST()
      IF(LTDST.NE.0)THEN
        CALL MZDROP(IXMAIN,LTDST,'l')
      END IF
      LTDST = 0
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL MZFORM('TDST','-F',IXIO)        ! Describe Bank format
      ENDIF
C
C--   FIND LINK TO SUPPORTING PARENT BANK
C
      LTTRH=GZTTRH()
C
      IF(LTTRH.LE.0)THEN
        CALL ERRMSG(' Problem booking TTRH','BKTDST',' ','W')
        GO TO 999
      END IF
      NL = XNL
      NS = XNS
      ND = XND
C
C ****
C
      CALL MZBOOK(IXMAIN,LTDST,LTTRH,-IZTDST,'TDST',NL,NS,ND,IXIO,0)
  999 RETURN
      END
