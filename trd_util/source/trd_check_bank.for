      SUBROUTINE TRD_CHECK_BANK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check integrity of TPRL bamk
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  25-FEB-1994   A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:zebcom.INC'
      INTEGER LTRDT,GZTRDT,LTPRL
      INTEGER ICH,LOUT,TRUNIT
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST)THEN
        FIRST=.FALSE.
        LOUT=TRUNIT()
      END IF
      LTRDT=GZTRDT()
      IF(LTRDT.LE.0)GO TO 999
      DO WHILE(LTRDT.NE.0)
        DO 10 ICH=1,3
          LTPRL=LQ(LTRDT-ICH)
          IF(LTPRL.LE.0)GO TO 10
c          PRINT*,' EVENT',IQ(LHEAD+9),'IQ(LTPRL+4)',IQ(LTPRL+4)
          IF(IQ(LTPRL+4).NE.394)THEN
            WRITE(LOUT,*)' EVENT',IQ(LHEAD+9),'IQ(LTPRL+4)',IQ(LTPRL+4)
          END IF
   10   CONTINUE
        LTRDT=LQ(LTRDT)
      END DO
  999 RETURN
      END
