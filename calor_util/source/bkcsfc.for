      SUBROUTINE BKCSFC(LCSFH1,LCSFC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the Bank CSFC
C-
C-   Inputs  : LCSFH1 = Link of parent bank.
C-                      = 0, will find it for you.
C-   Outputs : Link of Booked CSFC Bank
C-   Controls: None
C-
C-   Created   3-MAR-1992 09:30:41.38  Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LCSFC
      INTEGER LCSFH,LCSFH1
      INTEGER IXIO
      INTEGER GZCSFH,LSUP
C
C--   ZEBRA BANKS
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$LINKS:IZCSFC.LINK/LIST'
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
C--   INITIALIZE
C
      LCSFC = 0
      IF(FIRST)THEN
        CALL MZFORM('CSFC','2I -F',IXIO)        ! Describe Bank format
        FIRST = .FALSE.
      ENDIF
C
C--   FIND LINK TO SUPPORTING PARENT BANK
C
      IF ( LCSFH1.GE.0 ) THEN
        LCSFH = GZCSFH ()
      ELSE
        LCSFH = LCSFH1
      ENDIF
      IF ( LCSFH.EQ.0 ) CALL BKCSFH(LCSFH)
C
      LCSFC = LC(LCSFH-IZCSFC)
      IF(LCSFC.GT.0) THEN
        LSUP = LCSFC
   10   LCSFC = LC(LSUP)
        IF (LCSFC.GT.0) THEN
          LSUP = LCSFC 
          GOTO 10
        END IF
        CALL MZBOOK (IDVSTP,LCSFC,LSUP,0,'CSFC',1,1,4802,IXIO,0)
      ELSE
        CALL MZBOOK(IDVSTP,LCSFC,LCSFH,-IZCSFC,'CSFC',1,1,4802,IXIO,0)
      END IF
      IC(LCSFC+1) = 1                   ! version 1
  999 RETURN
      END
