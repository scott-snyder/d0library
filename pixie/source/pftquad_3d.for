      SUBROUTINE PFTQUAD_3D(HALF,QUAD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw outer boundary of theta quadrant in 3d.
C-   Amount of detail is determined by RCP parameter "FDC DRAW 3D DETAIL".
C-   More positive results in greater detail (from 1 to 3).
C-
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created  23-JUN-1992   Robert E. Avery
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$INC:PI.DEF'
C  LOCAL:
      INTEGER HALF, QUAD, SECTOR
      INTEGER POINT
      INTEGER GZFWTA, GZFWTB
      INTEGER LFWTX 
      INTEGER XOFFSET, YOFFSET 
      INTEGER LAYER 
      INTEGER FDC_QUADTYPE
      INTEGER DRAW_FULL
C
      REAL    XC, YC, DX, DY, ANG
      REAL    DIR, ZIN, ZOUT 
      REAL    DIMEN(6)
C
      INTEGER NPOINT 
      PARAMETER( NPOINT = 14 )
      REAL    UR(NPOINT),UL(NPOINT),V(NPOINT)
      REAL    UU(NPOINT),VV(NPOINT)
      REAL    ZZ(NPOINT)
C
      CHARACTER*4 FDCCLR
C----------------------------------------------------------------------
      CALL EZPICK('PX_FDCDIS_RCP')
      CALL PUGETA( 'FDC COLR BOUNDARY',FDCCLR )
      CALL PUGETV( 'FDC DRAW 3D DETAIL', DRAW_FULL)        
      CALL EZRSET
      CALL PXCOLR(FDCCLR )
      IF ( DRAW_FULL.EQ.0 ) GOTO 999
C
      IF(FDC_QUADTYPE(QUAD,HALF) .EQ. 1) THEN
        LFWTX = GZFWTA()
        XOFFSET = 2
        YOFFSET = 3
      ELSE
        LFWTX = GZFWTB()
        XOFFSET = 3
        YOFFSET = 2
      ENDIF
C
      IF(QUAD.LE.3) THEN
        ANG = -PI/4. + QUAD*HALFPI
        LAYER = 0
      ELSE
        ANG = -HALFPI + (QUAD-4)*HALFPI
        LAYER = 1
      ENDIF
C
C Get z
C
      DIR = (-1)**HALF * (-1)**LAYER 
      CALL GTFWAL(3*HALF+LAYER,DIMEN)
      ZIN = DIMEN(6) - DIR * C(LFWTX+9+4) 
      ZOUT = DIMEN(6) + DIR * C(LFWTX+9+4) 
C
C Accumulate Poly-line of quadrant outline:
C
      YC = C(LFWTX+9+YOFFSET+3)
      DY = C(LFWTX+9+YOFFSET)
      POINT = 1
      UR(POINT) = 0
      UL(POINT) = 0
      V(POINT) = YC-DY
C
      DO SECTOR =  0, MXSECT
        XC = C(LFWTX+9+9*SECTOR+XOFFSET+3)                  
        DX = C(LFWTX+9+9*SECTOR+XOFFSET)                    
        YC = C(LFWTX+9+9*SECTOR+YOFFSET+3)                  
        DY = C(LFWTX+9+9*SECTOR+YOFFSET)                    
C
        POINT = POINT + 1
        UR(POINT)=XC+DX
        UL(POINT)=-UR(POINT)
        V(POINT)=V(POINT-1)
C
        POINT = POINT + 1
        UR(POINT)=UR(POINT-1)
        UL(POINT)=-UR(POINT)
        V(POINT)=YC+DY
C
      ENDDO
C
      POINT = NPOINT
      UR(POINT)=XC
      UL(POINT)=UR(POINT)
      V(POINT)=V(POINT-1)
C
C  rotate the coordinate system by ANG
C
      DO POINT=1,NPOINT
        UU(POINT)=UR(POINT)*COS(ANG)-V(POINT)*SIN(ANG)
        VV(POINT)=UR(POINT)*SIN(ANG)+V(POINT)*COS(ANG)
        ZZ(POINT)=ZOUT
      ENDDO
      CALL J3MOVE(UU(1),VV(1),ZOUT)
      CALL J3POLY(UU,VV,ZZ,NPOINT)
C
C draw selected portions of box outline in more detail
C
      IF ( ABS(DRAW_FULL).GE.2 ) THEN
        DO POINT=NPOINT-4,NPOINT-1
          CALL J3MOVE(UU(POINT),VV(POINT),ZIN)
          CALL J3DRAW(UU(POINT),VV(POINT),ZOUT)
          ZZ(POINT)=ZIN
        ENDDO
        IF ( ABS(DRAW_FULL).EQ.2  ) THEN
          ZZ(NPOINT)=ZIN
          CALL J3MOVE(UU(NPOINT-4),VV(NPOINT-4),ZIN)
          CALL J3POLY(UU(NPOINT-4),VV(NPOINT-4),ZZ(NPOINT-4),5)
        ENDIF
      ENDIF
      IF ( ABS(DRAW_FULL).GE.3 ) THEN
        DO POINT=1,NPOINT
          ZZ(POINT)=ZIN
        ENDDO
        CALL J3MOVE(UU(1),VV(1),ZIN)
        CALL J3POLY(UU,VV,ZZ,NPOINT)
      ENDIF
C
C repeat for mirror image
C
      DO POINT=1,NPOINT
        UU(POINT)=UL(POINT)*COS(ANG)-V(POINT)*SIN(ANG)
        VV(POINT)=UL(POINT)*SIN(ANG)+V(POINT)*COS(ANG)
        ZZ(POINT)=ZOUT
      ENDDO
      CALL J3MOVE(UU(1),VV(1),ZOUT)
      CALL J3POLY(UU,VV,ZZ,NPOINT)
C
      IF ( ABS(DRAW_FULL).GE.2 ) THEN
        DO POINT=NPOINT-4,NPOINT-1
          CALL J3MOVE(UU(POINT),VV(POINT),ZIN)
          CALL J3DRAW(UU(POINT),VV(POINT),ZOUT)
          ZZ(POINT)=ZIN
        ENDDO
        IF ( ABS(DRAW_FULL).EQ.2  ) THEN
          ZZ(NPOINT)=ZIN
          CALL J3MOVE(UU(NPOINT-4),VV(NPOINT-4),ZIN)
          CALL J3POLY(UU(NPOINT-4),VV(NPOINT-4),ZZ(NPOINT-4),5)
        ENDIF
      ENDIF
      IF ( ABS(DRAW_FULL).GE.3 ) THEN
        DO POINT=1,NPOINT
          ZZ(POINT)=ZIN
        ENDDO
        CALL J3MOVE(UU(1),VV(1),ZIN)
        CALL J3POLY(UU,VV,ZZ,NPOINT)
      ENDIF
C
  999 RETURN
      END
