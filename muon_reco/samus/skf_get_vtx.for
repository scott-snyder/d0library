      SUBROUTINE  SKF_GET_VTX(VTX)
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      REAL    VTX(3)
      INTEGER I,J,LVERT,GZVERT
C---------------------------------------------------
C       Try VERT bank
C---------------------------------------------------
      LVERT = GZVERT(1)
      IF( LVERT.GT.0 ) THEN      
        VTX(1) = Q(LVERT+3)
        VTX(2) = Q(LVERT+4)
        VTX(3) = Q(LVERT+5)
        RETURN
      END IF
C---------------------------------------------------
C       Try ISAJET
C---------------------------------------------------
      CALL SKF_VERXYZ_GEANT(I,VTX,J)
      IF( J.GT.0 ) RETURN
C---------------------------------------------------
C       Use VERXYZ
C---------------------------------------------------
      I = 0
      CALL    VERXYZ(I,VTX,J)
      RETURN
      END
