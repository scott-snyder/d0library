      SUBROUTINE BKCADT(LCADT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the Bank CADT
C-
C-   Inputs  : none
C-   Outputs : Link of Booked CADT Bank
C-   Controls: None
C-
C-   Created  20-SEP-1990 10:20:16.41  Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LCADT,LSUP
      INTEGER IXIO
      INTEGER GZCGEH
C
C--   ZEBRA BANKS
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$LINKS:IZCADT.LINK/LIST'
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
C--   INITIALIZE
C
      LCADT = 0
      IF(FIRST)THEN
C
        FIRST= .FALSE.
        CALL MZFORM('CADT','-I',IXIO)        ! Describe Bank format
C
      ENDIF
C
C--   FIND LINK TO SUPPORTING PARENT BANK
C
      IF ( LCGEH.EQ.0 ) THEN
        CALL BKCGEH('STPC',LCGEH)
      ENDIF
C
C ****  LOOP OVER EXISTING CADT BANKS IN CHAIN
C
      LCADT = LC(LCGEH-IZCADT)
      IF(LCADT.GT.0) THEN
        LSUP = LCADT
   10   LCADT = LC(LSUP)
        IF (LCADT.GT.0) THEN
          LSUP = LCADT 
          GOTO 10
        END IF
        CALL MZBOOK (IDVSTP,LCADT,LSUP,0,'CADT',1,1,6150,IXIO,0)
      ELSE
        CALL MZBOOK(IDVSTP,LCADT,LCGEH,-IZCADT,'CADT',1,1,6150,IXIO,0)
        IC(LCADT+1) = 1                   ! version 1
      END IF
C
  999 RETURN
      END
