      SUBROUTINE BKEMAT(NDATA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the Bank EMAT
C-
C-   Inputs  : NDATA = Number of words to book in Bank
C-   Outputs : Link of Booked EMAT Bank
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
      INCLUDE 'D0$LINKS:IZEMAT.LINK/LIST'
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
      IF ( LEMAT.NE.0 ) THEN
        CALL ERRMSG('HMATRIX','BKEMAT',
     &    ' EMAT BANK ALREADY EXISTS ','W')
      ELSE
        IF(FIRST)THEN
C
          CALL MZFORM('EMAT','-D',IXIO)        ! Describe Bank format
C
        ENDIF
C
        CALL MZBOOK
     &  (IDVSTP,LEMAT,LHMTR,-IZEMAT,'EMAT',0,0,2*NDATA,IXIO,0)
C
C ****  2*NDATA for double precision
C
      ENDIF
  999 RETURN
      END
