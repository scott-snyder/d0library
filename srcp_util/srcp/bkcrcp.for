      SUBROUTINE BKCRCP(LSUP,IZLINK,ISTORE,ND,LCRCP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the Bank CRCP
C-
C-   Inputs  : LSUP   [I]   Address of suppporting bank
C-             IZLINK [I]   Link number to hang CRCP
C-             ISTORE [I]   Store index: 0=ZEBSTP, 1=ZEBCOM
C-             ND    = NUMBER OF WORDS IN CRCP
C-   Outputs : LCRCP  = Link of Booked CRCP Bank
C-   Controls: None
C-
C-   Created  13-JUN-1992   Chip Stewart   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LSUP,IZLINK,ISTORE,ND
      INTEGER LCRCP
C
C--   ZEBRA BANKS
C
      INTEGER NLINK(0:1),IXIO
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INCLUDE 'D0$INC:STP_ZLINKA.INC'
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
C--   INITIALIZE
C
      LCRCP = 0
      IF(FIRST)THEN
C
        CALL INZLNK
        CALL STP_INZLNK
        CALL GSLINK('EZSHUNT',NLINK(1))
        CALL STP_GSLINK('EZSHUNT',NLINK(0))
        IF(ISTORE.EQ.1) THEN
          LSLINK(NLINK(ISTORE)) = LSUP
        ELSE
          STP_LSLINK(NLINK(0)) = LSUP
        END IF
        CALL MZFORM('CRCP','-H',IXIO)        ! Describe Bank format
        IF(ISTORE.EQ.1) THEN
          LSUP = LSLINK(NLINK(ISTORE)) 
        ELSE
          LSUP = STP_LSLINK(NLINK(0)) 
        END IF
        CALL RSLINK('EZSHUNT',NLINK(1))
        CALL STP_RSLINK('EZSHUNT',NLINK(0))
C
      ENDIF
C
      IF (ISTORE.EQ.0) THEN     ! ZEBSTP
        CALL MZBOOK(IDVSTP,LCRCP,LSUP,-IZLINK,'CRCP',1,1,ND,IXIO,0)
      ELSE                      ! ZEBCOM
        CALL MZBOOK(IXMAIN,LCRCP,LSUP,-IZLINK,'CRCP',1,1,ND,IXIO,0)
      END IF
C
  999 RETURN
      END
