      SUBROUTINE BKCAGS(LCAGS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the Bank CAGS
C-
C-   Inputs  : none
C-   Outputs : Link of Booked CAGS Bank
C-   Controls: None
C-
C-   Created 25-APR-1991   James T. Linnemann   
C-   Updated  14-NOV-1993   James T. Linnemann  add energy offsets 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LCAGS
      INTEGER IXIO
C
C--   ZEBRA BANKS
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$LINKS:IZCAGS.LINK/LIST'
C
      CHARACTER*8 STIME
      INTEGER I, J, K, JDATE, JTIME
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
C----------------------------------------------------------------------
C
C--   Initialize
C
      LCAGS = 0
      IF(FIRST)THEN
C
        CALL MZFORM('CAGS','10I-F',IXIO)        ! Describe Bank format
        FIRST = .FALSE.
C
      ENDIF
C
C--   Find link to supporting parent bank
C
      IF ( LCGEH.EQ.0 ) CALL BKCGEH('STPC',LCGEH) 
      LCAGS = LC(LCGEH-IZCAGS)
      IF(LCAGS.LE.0) THEN
        CALL MZBOOK(IDVSTP,LCAGS,LCGEH,-IZCAGS,'CAGS',1,1,273,IXIO,0)
        CALL UCOPY(C(LSCAL+1),C(LCAGS+1),7)     !copy SCAL to CAGS
        IC(LCAGS+10) = 2                   ! version 2
C
C ****  Get date and time and pack into CGEH data
C
      CALL IDATE(I,J,K)
      JDATE = K + J*100 + I*10000
      CALL TIME(STIME)
      READ(UNIT=STIME,FMT='(I2,2(1X,I2))') I,J,K
      JTIME = K + J*100 + I*10000
C
      IC(LCAGS+7) = JDATE
      IC(LCAGS+8) = JTIME
      END IF
C
  999 RETURN
      END
