      SUBROUTINE MVER3(VER0,VER1,VER2,Y0,Y1,Y2,DEL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : LOOK AT TRIPLETS IN VERNIER VIEW
C-
C-   Inputs  : VER(2),Y  are 2 vernier solutions, wire position in Y
C-   Outputs : DEL = minimum miss distance
C
C-   Controls: 
C-
C-   Created  17-AUG-1988   David Hedin
C-     DH 6/89  ADD WAVELENGTH
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I0,I1,I2
      REAL VER0(2),VER1(2),VER2(2),Y0,Y1,Y2,DEL,DELMIN,SLOPE 
      REAL ZP
C
C   LOOP OVER COMBINATIONS
      DELMIN=999999.
      DO I0=1,2
        DO I1=1,2
          DO I2=1,2
            SLOPE=(VER2(I2)-VER0(I0))/(Y2-Y0)
CC  SKIP FUNNY ANGLES
            IF(ABS(SLOPE).LT.5.) THEN
              ZP=VER0(I0) + SLOPE*(Y1-Y0)
              DEL=VER1(I1) - ZP
              IF(ABS(DEL).LT.ABS(DELMIN)) THEN
                DELMIN=DEL
              ENDIF
            ENDIF
          ENDDO
        ENDDO
      ENDDO 
      DO I0=1,2
        DO I1=1,2
          DO I2=1,2
            SLOPE=(VER2(I2)-60.96-VER0(I0))/(Y2-Y0)
CC  SKIP FUNNY ANGLES
            IF(ABS(SLOPE).LT.5.) THEN
              ZP=VER0(I0)+60.96 + SLOPE*(Y1-Y0)
              DEL=VER1(I1) - ZP
              IF(ABS(DEL).LT.ABS(DELMIN)) THEN
                DELMIN=DEL
              ENDIF
            ENDIF
          ENDDO
        ENDDO
      ENDDO 
      DO I0=1,2
        DO I1=1,2
          DO I2=1,2
            SLOPE=(VER2(I2)-VER0(I0))/(Y2-Y0)
CC  SKIP FUNNY ANGLES
            IF(ABS(SLOPE).LT.5.) THEN
              ZP=VER0(I0) + SLOPE*(Y1-Y0)
              DEL=VER1(I1)+60.96 - ZP
              IF(ABS(DEL).LT.ABS(DELMIN)) THEN
                DELMIN=DEL
              ENDIF
            ENDIF
          ENDDO
        ENDDO
      ENDDO 
      DO I0=1,2
        DO I1=1,2
          DO I2=1,2
            SLOPE=(60.96+VER2(I2)-VER0(I0))/(Y2-Y0)
CC  SKIP FUNNY ANGLES
            IF(ABS(SLOPE).LT.5.) THEN
              ZP=VER0(I0) + SLOPE*(Y1-Y0)
              DEL=VER1(I1) - ZP
              IF(ABS(DEL).LT.ABS(DELMIN)) THEN
                DELMIN=DEL
              ENDIF
            ENDIF
          ENDDO
        ENDDO
      ENDDO 
C
      DEL=DELMIN
C----------------------------------------------------------------------
  999 RETURN
      END
