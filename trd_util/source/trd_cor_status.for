      INTEGER FUNCTION TRD_COR_STATUS(LAYER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : calculates status word to be written in TPRL
C-   Returned value  : status word 
C-          bit 1 (LSB) : electronic gain         (0=none,1=done)
C-          bit 2       : epicor calibration      (0=none,1=done)
C-          bit 3       : APC pedestal correction (0=none,1=done)
C-          bit 4       : pedestal substraction   (0=none,1=done)
C-          bit 5       : sector correction       (0=none,1=done)
C-          bit 6       : wire correction         (0=none,1=done)
C-          bit 7       : hv correction           (0=none,1=done)
C-          bit 8       : angular correction      (0=none,1=done)
C-          bit 9       : gas correction          (0=none,1=done)
C-   Inputs  : LAYER      integer   (1,2,3)
C-             (reads in common TRDCOR)
C-   Outputs : none
C-   Controls: none
C-
C-   Created  19-JAN-1993   Alain PLUQUET
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:TRDCOR.INC'
      INTEGER I,J,K,WIRE,LAYER
      LOGICAL OK(9)
      J=0
      DO K=1,9
        OK(K)=.TRUE.
      ENDDO
      DO WIRE=1,4
        DO K=1,9
          OK(K)=OK(K).AND.STATUS(LAYER,WIRE,K).EQ.0
        ENDDO
      ENDDO
      DO K=1,9
        IF (OK(K)) THEN
          CALL SBIT1(J,K)
        ELSE
          CALL SBIT0(J,K)
        ENDIF
      ENDDO
      TRD_COR_STATUS=J
      END
