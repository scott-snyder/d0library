      SUBROUTINE PT1WIR(WIRE,LAYER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To display the FADC traces of a a specific
C-            wire and layer for the TRD's.
C-
C-   Inputs  : WIRE - wire number
C-             LAYER- layer number, if layer equal 0 then all 3 layers 
C-                    of the wire will be displayed
C-
C-
C-   Created  21-DEC-1988   LUPE ROSAS
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER WIRE,LAYER,TMAX
      PARAMETER(TMAX=15)
      INTEGER TLAY(TMAX),TWIR(TMAX),NTFADC,I,NUMLAY
      REAL TFADC(128,TMAX)
      LOGICAL NHITWI        ! FLAG FOR NO HIT WIRE
C----------------------------------------------------------------------
      NHITWI=.TRUE.
      IF ((LAYER.GT.3).OR.(LAYER.LT.0)) GO TO 400   ! Checking LAYER num
      IF((WIRE.GE.1).AND.(WIRE.LE.256)) THEN        ! Checking the WIRE num
        CALL PTGTRD(TMAX,NTFADC,WIRE,TLAY,TWIR,TFADC,NHITWI) ! Gets the
C    ! info of the common blocks for a particular wire (WIRE) and/or layer num
        IF (NHITWI) THEN     ! WIRER NOT HIT
          GO TO 400
        ENDIF
      ELSE   
        GO TO 400    ! ILEGAL WIRE NUM
      ENDIF    
      NUMLAY = LAYER  ! Setting the layer to be disp
      CALL PUHEAD('PTADC1')
      CALL PTCHAN(NTFADC,TLAY,TWIR,TFADC,NUMLAY)
  400 RETURN
      END
