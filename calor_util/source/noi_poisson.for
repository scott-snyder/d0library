      SUBROUTINE NOI_POISSON
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     GENERATES POISSON DISTRIBUTION POIS(JJ) AND ITS INTEGRAL INTEG(JJ)
C      FOR AVERAGE=A.....JJ=1=ZERO HITS, JJ=21=20HITS
C
C-
C-   Inputs  : OCCUPY = average value of distribution
C-   Outputs : INTEG(21)=integral of poisson distribution
C-             from 0 to 20 HITS
C-   Controls:
C-
C-   Created   5-SEP-1991  Peter Nemethy and Allen I. Mincer
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:NOISY.INC'
      REAL A
      REAL POIS(21),ANFAC
      INTEGER JJ
C
C#######EXECUTION BEGINS#####
C
      A=OCCUPY
      ANFAC=1.0
      POIS(1)=EXP(-A)
      INTEG(1)=POIS(1)
      DO JJ=2,21
        ANFAC=ANFAC*FLOAT(JJ-1)
        POIS(JJ)=EXP(-A)*A**(JJ-1)/ANFAC
        INTEG(JJ)=INTEG(JJ-1)+POIS(JJ)
      ENDDO
      IF(PRINT_OUTPUT)THEN
        DO JJ=1,21
          IF(POIS(JJ).GT.1.E-8)THEN
            WRITE(OUTUNI,*)' JJ-1,POISSON,INTEG',
     &                    JJ-1,POIS(JJ),INTEG(JJ)
          ENDIF
        ENDDO
      ENDIF
      RETURN
      END
