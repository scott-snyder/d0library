      SUBROUTINE BKCSFW(LCSFH1,LCSFW)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the Bank CSFW
C-
C-   Inputs  : LCSFH1 = Link of parent bank.
C-                      = 0, will find it for you.
C-   Outputs : Link of Booked CSFW Bank
C-   Controls: None
C-
C-   Created   3-MAR-1992 09:30:41.38  Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LCSFW
      INTEGER LCSFH,LCSFH1
      INTEGER IXIO
      INTEGER GZCSFH
C
C--   ZEBRA BANKS
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$LINKS:IZCSFW.LINK/LIST'
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
C--   INITIALIZE
C
      LCSFW = 0
      IF(FIRST)THEN
        CALL MZFORM('CSFW','1I -F',IXIO)        ! Describe Bank format
        FIRST = .FALSE.
      ENDIF
C
C--   FIND LINK TO SUPPORTING PARENT BANK
C
      IF ( LCSFH1.EQ.0 ) THEN
        LCSFH = GZCSFH ()
      ELSE
        LCSFH = LCSFH1
      ENDIF
      IF ( LCSFH.EQ.0 ) CALL BKCSFH(LCSFH)
C
      CALL MZBOOK
     &  (IDVSTP,LCSFW,LCSFH,-IZCSFW,'CSFW',1,1,630,IXIO,0)
C
      IC(LCSFW+1) = 1  !version 1
  999 RETURN
      END
