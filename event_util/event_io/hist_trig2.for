      SUBROUTINE HIST_TRIG2(ID)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Make a histogram of level 2 trigger bits in
C-                         histogram ID in current directory
C-
C-   Inputs  : ID   HBOOK id of histogram to fill
C-   Outputs : 
C-   Controls: 
C-
C-   Created  29-MAY-1992   K. Wyatt Merritt
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      LOGICAL BTEST
C
      INTEGER ID
      INTEGER IMASK,I,N
C
C----------------------------------------------------------------------
      DO N = 0,3
        IMASK = IQ(LHEAD + 15 + N)
        DO I = 0,31
          IF (BTEST(IMASK,I)) CALL HF1(ID,FLOAT(I+32*N),1.)
        ENDDO
      ENDDO
  999 RETURN
      END
