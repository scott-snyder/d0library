      SUBROUTINE L2_EM_CUT_BINS(E_EM,IETAC,JENRG_BIN,JETA_BIN,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : quantize energy and eta into bins for picking cuts
C-
C-   Inputs  : E_EM         The EM energy in GeV
C-             IETAC        The offline eta coordinate of the candidate
C-   Outputs : JENRG_BIN    The energy index of the cut [1-3]
C-             JETA_BIN     The eta index of the cut [1-8]
C-             OK           .TRUE. if the input coordinates are OK and have cuts
C-                            defined.  At large eta, there are no cuts defined.
C-   Controls:
C-
C-   Created  17-DEC-1991   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:l2_EM.PARAMS'
      INCLUDE 'D0$INC:L2_EM_STP.INC'
      REAL E_EM
      INTEGER I,IETAC,JENRG_BIN,JETA_BIN
      LOGICAL OK
      INTEGER CC_EC
C----------------------------------------------------------------------
      OK = .TRUE.
      IF(IABS(IETAC).LE.13)THEN
        CC_EC = 1
      ELSE
        CC_EC = 2
      ENDIF
C
C...find energy bin
      DO I = 1,NENRGBIN-1
        IF(E_EM.LE.E_BOUNDS(I,CC_EC))THEN
          JENRG_BIN = I
          GOTO 50
        ENDIF
      ENDDO
      JENRG_BIN = NENRGBIN  !in the top bin
   50 CONTINUE
C
C
C...find eta bin of candidate for cuts
      DO I = 1,NETABIN
        IF(IABS(IETAC).LE.ETA_BOUNDS(I))THEN
          JETA_BIN = I
          GOTO 100
        ENDIF
      ENDDO
      OK = .FALSE.     !nothing to say if beyond last defined region
      GO TO 999
  100 CONTINUE
      IF (JETA_BIN.EQ.4) THEN
        OK = .FALSE.  !nothing to say if hit region with no EM channels
        GO TO 999
      ENDIF
  999 RETURN
      END
