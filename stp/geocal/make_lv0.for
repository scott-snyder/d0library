      SUBROUTINE MAKE_LV0(LUNLV0)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : MAKE SRCP FILE FOR LV0 GEOMETRY
C-
C-   Inputs  : LUNLV0 OUTPUT UNIT NUMBER
C-   Outputs : NONE
C-   Controls: SRCP_RAW_LV0.DAT
C-
C-   Created  12-JAN-1989   Chip Stewart
C-   Updated  14-FEB-1992   Freedy Nang
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*1 ZEE(2)
      CHARACTER*32 NAME
      INTEGER LUNLV0, IZ
      REAL INCH_CM
      INTEGER LVAL, IER, M, N, P
C
C----------------------------------------------------
C
C ****  GET DATA FROM SRCP
C
      CALL EZGET('CONVERSION_FACTOR',INCH_CM,IER)
      DO IZ= 1,2
        CALL EZGETS('ZEE',IZ,NAME,LVAL,IER)
        NAME=NAME(1:LVAL)
        CALL SWORDS(NAME,M,N,P)
        ZEE(IZ)=NAME(M:N)
      ENDDO
C
C ****  CREATE LEVEL 0
C
      WRITE(LUNLV0,1)
      CALL MAKE_LV0_MOTHER(INCH_CM,ZEE,LUNLV0)
      CALL MAKE_LV0_TILE(INCH_CM,ZEE,LUNLV0)
      CALL MAKE_LV0_COVER(INCH_CM,ZEE,LUNLV0)
      CALL MAKE_LV0_PMT(INCH_CM,ZEE,LUNLV0)
      CALL MAKE_LV0_MIXTURE(LUNLV0)
      CALL MAKE_LV0_SET(ZEE,LUNLV0)
      WRITE(LUNLV0,2)
    1 FORMAT(1X,'\START')
    2 FORMAT(1X,'\STOP')
  999 RETURN
      END
