      FUNCTION NOI_HIT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   GENERATES A RANDOM INTEGER NOI_HIT ACCORDING TO POISSON DISTRIBUTION
C-   WHOSE INTEGRAL IS INTEG(21), WHERE INTEG(1) IS ZERO HITS!
C-
C-   Inputs  :  INTEG(1:21) = integral Poisson probability
C-                distribution of 0 - 20 hits
C-   Outputs :  NOI_HIT = picked number
C-   Controls: None
C-
C-   Created   17-SEP-1991   Peter Nemethy and Allen I. Mincer
C-   Modified   8-MAR-1993   Allen I. Mincer:  Protect against JJ>21
C-                              Add FIXNUM switch for always having
C-                              FIXNUM events
C-   Modified  19-SEP-1994   Allen I. Mincer:  Made consistent with RNDM
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:NOISY.INC'
      INTEGER JJ,NOI_HIT
      REAL ARG,RAN,RNDM,RSEED
C
C#######EXECUTION BEGINS#####
C
      IF(FIXNUM.GT.0)THEN
        NOI_HIT=FIXNUM
      ELSE
C         VAX VERSION:
C          ARG=RAN(RAN_VALUE)
C         RNDM:
        RSEED=FIXNUM
        ARG=RNDM(RSEED)
        DO JJ=1,21
          IF(ARG.LT.INTEG(JJ))GO TO 1
        ENDDO
        JJ=21
    1   NOI_HIT=JJ-1
      ENDIF
      RETURN
      END
