      SUBROUTINE BKL2CR(LL2CR,NDATA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book L2CR bank that hangs below SL2H
C-
C-   Inputs  : NDATA    Number of Data words to be booked
C-   Outputs : LL2CR    Link to LL2CR bank
C-   Controls:
C-
C-   Created  13-NOV-1990   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZL2CR.LINK'
      INTEGER LL2CR,GZSL2H,LSL2H,NDATA
C----------------------------------------------------------------------
      LSL2H = GZSL2H()
      IF (LSL2H .LE. 0) CALL BKSL2H(LSL2H)
C
      LL2CR = LC(LSL2H - IZL2CR)
      IF (LL2CR .LE. 0) THEN
        CALL MZBOOK(IDVSTP,LL2CR,LSL2H,-IZL2CR,'L2CR',0,0,NDATA,2,0)
        IC(LL2CR + 1) = 1               ! Version number
        IC(LL2CR + 4) = NDATA           ! Number of data words
      END IF



  999 RETURN
      END
