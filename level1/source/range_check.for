      SUBROUTINE RANGE_CHECK (ETA1, ETA2, ETA_SIGN, PHI1, PHI2, ERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Checks if eta and phi ranges are legal.
C-
C-   Inputs  : ETA1, ETA2 : eta range bounds;
C-             PHI1, PHI2 : phi range bounds.
C-
C-   Outputs : ETA1, ETA2 : eta range ordered bounds (0<ETA1<=ETA2);
C-             ETA_SIGN : eta sign;
C-             PHI1, PHI2 : phi range ordered bounds (PHI1<=PHI2);
C-             ERR : Error code.
C-
C-   Controls: None.
C-
C-   Created   7-FEB-1990   Sylvain Tisserant (MSU)
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$INC:INTERPRETER_CODES.INC'
C
      INTEGER  ETA1, ETA2, ETA_SIGN, PHI1, PHI2, ERR
C
      INTEGER  I
C
C----------------------------------------------------------------------
C
      IF((ETA1*ETA2).LE.0) GOTO 10
      IF(ETA1.LT.0) THEN
        ETA_SIGN = NEG_ETA
        ETA1     = -ETA1
        ETA2     = -ETA2
      ELSE
        ETA_SIGN = POS_ETA
      ENDIF
      IF(ETA1.GT.ETA2) THEN
        I    = ETA1
        ETA1 = ETA2
        ETA2 = I
      ENDIF
      IF((ETA1.GE.ETA_MIN).AND.(ETA2.LE.ETA_MAX)) GOTO 20
   10 ERR = PARSER_ETA_OUT_OF_RANGE
      RETURN
C
   20 IF(PHI1.GT.PHI2) THEN
        I    = PHI1
        PHI1 = PHI2
        PHI2 = I
      ENDIF
      IF((PHI1.GE.PHI_MIN).AND.(PHI2.LE.PHI_MAX)) THEN
        ERR = PARSER_SUCCESS
      ELSE
        ERR = PARSER_PHI_OUT_OF_RANGE
      ENDIF
      RETURN
      END
