      SUBROUTINE C_SETUP_ZTRAKS(IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : SETUP ZTRAKS INFO INTO CTRAK.INC
C-
C-   Inputs  :
C-   Outputs : IER = 1 no ELECTRON track. =2 No zfit
C-   Controls:
C-
C-   Created  11-SEP-1992   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:CTRAK.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$LINKS:IZZFIT.LINK'
      INTEGER IER
C----------------------------------------------------------------------
C
C ****  WE NEED TO PICK UP PRIMARY ELECTRON PARAMETERS
C
      IER = 0
      IF(LZTRAK_ELECTRON.EQ.0)THEN
      IER = 1
      RETURN
      ELSE
        LZFIT = LQ(LZTRAK_ELECTRON-IZZFIT)
        IF(LZFIT.EQ.0)THEN
          IER = 2
          RETURN
        ELSE
          UVEC(1,1) = Q(LZFIT+20)
          UVEC(2,1) = Q(LZFIT+22)
          UVEC(3,1) = Q(LZFIT+24)
C
          VERT(1) = Q(LZFIT+11)  !TRACK CENTER FOR VERTEX NOW
          VERT(2) = Q(LZFIT+12)  !TRACK CENTER FOR VERTEX NOW
          VERT(3) = Q(LZFIT+15)  !TRACK CENTER FOR VERTEX NOW
C
          PHI(1) = ATAN2(UVEC(2,1),UVEC(1,1))/RADIAN     ! IN DEGREES
          RAP(1) = ATAN2(SQRT(UVEC(2,1)**2+UVEC(1,1)**2),UVEC(3,1))
          RAP(1) = -ALOG(SIN(RAP(1)/2.)/COS(RAP(1)/2.))    ! RAPIDITY OF TRACK
        ENDIF
      ENDIF
C
  999 RETURN
      END
