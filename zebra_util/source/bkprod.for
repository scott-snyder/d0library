      SUBROUTINE BKPROD(NDATA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the Bank PROD
C-
C-   Inputs  : NDATA = Number of words to book in Bank
C-   Outputs : Link of Booked PROD Bank
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
      INCLUDE 'D0$LINKS:IZPROD.LINK/LIST'
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
      IF ( LPROD.NE.0 ) THEN
        CALL ERRMSG('HMATRIX','BKPROD',
     &    ' PROD BANK ALREADY EXISTS ','W')
      ELSE
        IF(FIRST)THEN
C
          CALL MZFORM('PROD','-D',IXIO)        ! Describe Bank format
C
        ENDIF
C
        CALL MZBOOK
     &  (IDVSTP,LPROD,LHMTR,-IZPROD,'PROD',0,0,2*NDATA,IXIO,0)
C
C ****  2*NDATA for double precision
C
      ENDIF
  999 RETURN
      END
