      SUBROUTINE BKL0VT(LL0VT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Books l0VT bank, level0 vertex data. 
C-
C-   Inputs  : none
C-   Outputs : LL0VT= point to L0VT
C-   Controls: none
C-
C-   Created   4-JUN-1992   Tom Fahland
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$links:IZL0VT.LINK'             !not there yet  
      INTEGER LL0VT,POINT,NDATA
      INTEGER  GZFRES,IOL0VT
      INTEGER NSIZ                      ! Total number of data words
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------

      NDATA = 5                  !# of data words

      IF ( FIRST ) THEN
        CALL MZFORM('L0VT','1I 1F 1I 1F 1I',IOL0VT)        
        FIRST = .FALSE.
      END IF

cccc  Find link to supporting parent bank  ccccccc
      POINT =GZFRES()
cccc Book it  ccccccc

      IF (POINT .LE. 0) CALL BKFRES(POINT)
      CALL MZBOOK(IXMAIN,LL0VT,POINT,-IZL0VT,'L0VT',0,0,NDATA,IOL0VT,0)
      IQ(LL0VT+1) = 1   !version 1
C...other words initialized to zero

  999 RETURN
      END
