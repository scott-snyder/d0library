      SUBROUTINE LSQ_READ_EVENT(UNIT,EOF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   3-MAR-1992   Rajendran Raja
C-   Updated   2-Feb-1993   Herbert Greenlee
C-      Removed BLOCK DATA subprogram
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:LSQ_EVENT.INC'
      INTEGER UNIT
      LOGICAL EOF
      INTEGER I
C----------------------------------------------------------------------
      EOF = .FALSE.
      READ(UNIT,*,ERR=998,END=998) ETOT
C
      NEV=NEV+1
C
      ETOT(3)=ETOT(3)+ETOT(4)+ETOT(5)+ETOT(6)
      ETOT(11)=ETOT(11)+ETOT(12)+ETOT(13)+ETOT(14)+
     &    ETOT(15)+ETOT(16)+ETOT(17)
C
      ETOT(22)=ETOT(22)+ETOT(23)+ETOT(24)+ETOT(25)
C
      E0=0.
      DO I=1,NLIVE
        E0=E0+ETOT(LYR_LIVE(I))+ETOT(LYR_DEAD(I))
        ELIVE(I) = ETOT(LYR_LIVE(I))
        EDEAD(I) = ETOT(LYR_DEAD(I))
      ENDDO
C
  999 RETURN
  998 EOF = .TRUE.
      RETURN
      END
