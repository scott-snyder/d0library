      SUBROUTINE NOI_PILEUP_STAT(IFLAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Do statistics on pileup
C-
C-   Inputs  :
C-   Outputs :
C-   Controls: IFLAG = 0 accumulate statistics
C-                     1 print out statistics
C-
C-   Created  25-SEP-1991   Allen I. Mincer
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:NOISY.INC'
      INTEGER IFLAG,MXKFIN
      REAL ANEV,AKFIN,AKSQ,AVK,AVSQ,SIGK
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST)THEN
        FIRST=.FALSE.
        ANEV=0.0
        AKFIN=0.0
        MXKFIN=0
        AKSQ=0.0
      ENDIF
      IF(IFLAG.EQ.0)THEN
        ANEV=ANEV+1.0
        AKFIN=AKFIN+FLOAT(KFIN)
        AKSQ=AKSQ+(FLOAT(KFIN))**2
        AVK=AKFIN/ANEV
        AVSQ=AKSQ/ANEV
        SIGK=SQRT(AVSQ-AVK**2)
        IF(KFIN.GT.MXKFIN)MXKFIN=KFIN
      ELSEIF(IFLAG.EQ.1)THEN
        IF(PRINT_OUTPUT)THEN
          WRITE(OUTUNI,1000)ANEV,AVK,SIGK,MXKFIN
 1000     FORMAT(' NUM EVENTS, AV NUM PILEUP,SIGMA,MAX ',
     &          F10.1,2F10.4,I5)
        ENDIF
      ENDIF
  999 RETURN
      END
