      SUBROUTINE CPTCTZ 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     Zeroes pointers for the CATE bank if it exists already 
C-
C-   Created   19-APR-1989   Andrzej Zieminski
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PTCATE.INC'
C
      INTEGER I 
      INTEGER IETA,IPHI,IHD,POINTR
      INTEGER NCH,NR,NTOT 
C----------------------------------------------------------------------
C
      IF(PTTFLG) GOTO 999    ! PTCATE array has not been done --> do nothing
C
C        initialize
C
      PTTFLG=.TRUE.
      NTOT=(2*NETAL+1)*NPHIL*2
      CALL UZERO(PTCATE,1,NTOT)
C
  999 RETURN
      END
