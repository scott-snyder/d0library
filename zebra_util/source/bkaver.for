      SUBROUTINE BKAVER(NDATA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the Bank AVER
C-
C-   Inputs  : NDATA = Number of words to book in Bank
C-   Outputs : Link of Booked AVER Bank
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
      INCLUDE 'D0$LINKS:IZAVER.LINK/LIST'
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
      IF ( LAVER.NE.0 ) THEN
        CALL ERRMSG('HMATRIX','BKAVER',
     &    ' AVER BANK ALREADY EXISTS ','W')
      ELSE
        IF(FIRST)THEN
C
          CALL MZFORM('AVER','-D',IXIO)        ! Describe Bank format
C
        ENDIF
C
        CALL MZBOOK
     &  (IDVSTP,LAVER,LHMTR,-IZAVER,'AVER',0,0,2*NDATA,IXIO,0)
C
C ****  2*NDATA for double precision
C
C
      ENDIF
  999 RETURN
      END
