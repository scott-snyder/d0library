      SUBROUTINE MVER4(V0,V1,V2,V3,Y0,Y1,Y2,Y3,CHI)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : LOOK AT 4-HITS IN VERNIER VIEW
C-
C-   Inputs  : Vi(2),Y  are 2 vernier solutions, wire position in Y
C-   Outputs : CHI = MINIMUM QUALITY OF FIT =SQRT(RESIDUAL SUM**2/2)
C
C-  NOTE THAT THIS FOLLOWS MVER3 IN THAT IT LOOKS AT VARIOUS POSSIBLE 
C-  wavelength combinations (THOUGH NOT ALL ARE DONE)
C-   Created  22-JAN-1990   David Hedin
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I0,I1,I2,I3
      REAL V0(2),V1(2),V2(2),V3(2),Y0,Y1,Y2,Y3 
      REAL ZP,CHI,CHIMIN,SX,SY,SX2,SY2,SXY,A,B,C,D,E
C
C   LOOP OVER COMBINATIONS
      SY=Y0+Y1+Y2+Y3
      SY2=Y0**2+Y1**2+Y2**2+Y3**2
      CHIMIN=999999.
      DO I0=1,2
        DO I1=1,2
          DO I2=1,2
            DO I3=1,2
              SX=V0(I0)+V1(I1)+V2(I2)+V3(I3) 
              SX2=V0(I0)**2+V1(I1)**2+V2(I2)**2+V3(I3)**2
              SXY=V0(I0)*Y0+V1(I1)*Y1+V2(I2)*Y2+V3(I3)*Y3
              CALL MUSLIN(4.,SX,SY,SX2,SY2,SXY,A,B,C,D,E,CHI)
              IF(ABS(CHI).LT.ABS(CHIMIN)) THEN
                CHIMIN=CHI
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDDO 
      DO I0=1,2
        DO I1=1,2
          DO I2=1,2
CCCC     ADD 1 WAVELENGTH TO PLANE 0
            DO I3=1,2
              SX=V0(I0)+V1(I1)+V2(I2)+V3(I3)+60.96
              SX2=(V0(I0)+60.96)**2+V1(I1)**2+V2(I2)**2+V3(I3)**2
              SXY=(V0(I0)+60.96)*Y0+V1(I1)*Y1+V2(I2)*Y2+V3(I3)*Y3
              CALL MUSLIN(4.,SX,SY,SX2,SY2,SXY,A,B,C,D,E,CHI)
              IF(ABS(CHI).LT.ABS(CHIMIN)) THEN
                CHIMIN=CHI
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDDO 
      DO I0=1,2
        DO I1=1,2
          DO I2=1,2
CCCC     ADD 1 WAVELENGTH TO PLANE 1
            DO I3=1,2
              SX=V0(I0)+V1(I1)+V2(I2)+V3(I3)+60.96
              SX2=V0(I0)**2+(V1(I1)+60.96)**2+V2(I2)**2+V3(I3)**2
              SXY=V0(I0)*Y0+(V1(I1)+60.96)*Y1+V2(I2)*Y2+V3(I3)*Y3
              CALL MUSLIN(4.,SX,SY,SX2,SY2,SXY,A,B,C,D,E,CHI)
              IF(ABS(CHI).LT.ABS(CHIMIN)) THEN
                CHIMIN=CHI
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDDO 
      DO I0=1,2
        DO I1=1,2
          DO I2=1,2
CCCC     ADD 1 WAVELENGTH TO PLANE 2
            DO I3=1,2
              SX=V0(I0)+V1(I1)+V2(I2)+V3(I3)+60.96
              SX2=V0(I0)**2+V1(I1)**2+(V2(I2)+60.96)**2+V3(I3)**2
              SXY=V0(I0)*Y0+V1(I1)*Y1+(V2(I2)+60.96)*Y2+V3(I3)*Y3
              CALL MUSLIN(4.,SX,SY,SX2,SY2,SXY,A,B,C,D,E,CHI)
              IF(ABS(CHI).LT.ABS(CHIMIN)) THEN
                CHIMIN=CHI
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDDO 
      DO I0=1,2
        DO I1=1,2
          DO I2=1,2
CCCC     ADD 1 WAVELENGTH TO PLANE 3
            DO I3=1,2
              SX=V0(I0)+V1(I1)+V2(I2)+V3(I3)+60.96
              SX2=V0(I0)**2+V1(I1)**2+V2(I2)**2+(V3(I3)+60.96)**2
              SXY=V0(I0)*Y0+V1(I1)*Y1+V2(I2)*Y2+(V3(I3)+60.96)*Y3
              CALL MUSLIN(4.,SX,SY,SX2,SY2,SXY,A,B,C,D,E,CHI)
              IF(ABS(CHI).LT.ABS(CHIMIN)) THEN
                CHIMIN=CHI
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDDO 
C
      CHI=CHIMIN
C----------------------------------------------------------------------
  999 RETURN
      END
