      SUBROUTINE BKTEMP(NDATA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the Bank TEMP
C-
C-   Inputs  : NDATA = Number of words to book in Bank
C-   Outputs : Link of Booked TEMP Bank
C-   Controls: None
C-
C-   Created  20-DEC-1990 15:07:37.26  Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IXIO
C
C--   ZEBRA BANKS
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
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
      IF ( LTEMP.NE.0 ) THEN
        CALL ERRMSG('HMATRIX','BKTEMP',
     &    ' TEMP BANK ALREADY EXISTS ','W')
      ELSE
        IF(FIRST)THEN
C
          CALL MZFORM('TEMP','-F',IXIO)        ! Describe Bank format
C
        ENDIF
C
        CALL MZBOOK
     &  (IDVSTP,LTEMP,0,2,'TEMP',0,0,NDATA,IXIO,0)
C
      ENDIF
  999 RETURN
      END
