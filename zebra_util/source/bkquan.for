      SUBROUTINE BKQUAN(NDATA)
C----------------------------------------------------------------------
C-
C-
C-   Purpose and Methods : Book the Bank QUAN
C-
C-   Inputs  : NDATA = Number of words to book in Bank
C-   Outputs : Link of Booked QUAN Bank
C-   Controls: None
C-
C-
C-   Created  22-DEC-1990 14:56:34.54  Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IXIO
C
C--   ZEBRA BANKS
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$LINKS:IZQUAN.LINK/LIST'
      INCLUDE 'D0$INC:ZHMATRIX.INC'
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      INTEGER NDATA
C----------------------------------------------------------------------
C
C--   INITIALIZE
C
      IF ( LQUAN.NE.0 ) THEN
        CALL ERRMSG('HMATRIX','BKQUAN',
     &    ' QUAN BANK ALREADY EXISTS ','W')
      ELSE
        IF(FIRST)THEN
C
          CALL MZFORM('QUAN','-F',IXIO)        ! Describe Bank format
C
        ENDIF
C
        CALL MZBOOK
     &  (IDVSTP,LQUAN,LHMTR,-IZQUAN,'QUAN',0,0,NDATA,IXIO,0)
C
      ENDIF
  999 RETURN
      END
