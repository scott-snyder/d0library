      SUBROUTINE PTUSEV(NEVT)
C     ******************************************************************
C
C     For communicate the variable NEVT ( user event number )
C     INPUT : NEVT
C     ENTRY GTUSEV
C           OUTPUT : NEVT
C
C                                                 H.WANG   June 15,87
C
C     ******************************************************************
      IMPLICIT NONE
      INTEGER NEVT,N
 
      N = NEVT
      RETURN

      ENTRY GTUSEV(NEVT)
      NEVT = N
      RETURN
      END
