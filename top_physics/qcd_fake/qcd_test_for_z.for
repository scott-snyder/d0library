      SUBROUTINE QCD_TEST_FOR_Z(IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TEST WHETHER TWO ELECTRONS FALL WITHIN THE Z RANGE
C-
C-   Inputs  :
C-   Outputs : IER NOT ZERO IF Z
C-   Controls:
C-
C-   Created   4-APR-1994   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:QCD_SMEAR_EVENT.INC'
      INCLUDE 'D0$INC:QCD_FAKE_NTUPLE.INC'
      INTEGER IER
      DOUBLE PRECISION ESUM(5)
      REAL    ZLIM(2)
      LOGICAL first
      SAVE first
      DATA first / .true. /
C----------------------------------------------------------------------
      IF( first ) THEN
        first = .false.
        CALL EZPICK('QCD_FAKE_RCP')
        CALL EZGET('ZMASS_LIMITS',ZLIM,IER)
        CALL EZRSET
      ENDIF
C
      IF ( NELE.GE.2 ) THEN
        CALL PAIR_MASSD(ELEC(1,1),ELEC(1,2),ESUM)
C
        ZMASS = ESUM(5)
        IF ( ZMASS.GE.ZLIM(1).AND.ZMASS.LE.ZLIM(2)
     &    .AND.BAD_ELE.EQ.0.0 ) THEN
          IER = 1
        ELSE
          IER = 0
        ENDIF
      ELSE
        ZMASS = 0.0
        IER = 0
      ENDIF
C
  999 RETURN
      END
