      SUBROUTINE BKUMAT(NDATA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book the Bank UMAT
C-
C-   Inputs  : NDATA = Number of words to book in Bank
C-   Outputs : Link of Booked UMAT Bank
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
      INCLUDE 'D0$LINKS:IZUMAT.LINK/LIST'
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
      IF ( LUMAT.NE.0 ) THEN
        CALL ERRMSG('HMATRIX','BKUMAT',
     &    ' UMAT BANK ALREADY EXISTS ','W')
      ELSE
        IF(FIRST)THEN
C
          CALL MZFORM('UMAT','-F',IXIO)        ! Describe Bank format
C
        ENDIF
C
        CALL MZBOOK
     &  (IDVSTP,LUMAT,LHMTR,-IZUMAT,'UMAT',0,0,NDATA,IXIO,0)
C
      ENDIF
  999 RETURN
      END
