      SUBROUTINE VTXC_MATCH_ZTRAK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compare CDC or FDC tracks with VTX tracks
C-                         and match them.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  27-APR-1992   Myungyun Pang
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER I_TRAK
      INTEGER LZTRK,LVTRH,GZVTRH,LSEG,LZFIT
      INTEGER LFTRH,GZFTRH,N_FDC_TRAK,LFDCT
      INTEGER GZFDCT,N_VTX_TRAK,LVTXT,GZVTXT
      INTEGER K_TRAK
      INTEGER GZZTRK,LZTRH,GZZTRH
      INTEGER N_ZTRAK,NZBANK,N_TRACK,LUN0,LUN1
      INTEGER GZVERT,LVERT,LVERH,GZVERH,N_PVERT
      INTEGER EVENT,N_VTX_TRACK,EVONUM
      INTEGER LAYER,LAY,ID,IER
      INTEGER MX_TRK_LAY
      PARAMETER ( MX_TRK_LAY = 250 )
      INTEGER NTRAK(0:2), TRKNUM(MX_TRK_LAY,0:2)
      INTEGER INDEX(MX_TRK_LAY,0:2), CLOSEST
      REAL TRKPHI(MX_TRK_LAY,0:2), NEW_DPHI, OLD_DPHI, PHI2PI, ADJACENT
      REAL PHI,MIN_DPHI,MIN_DPHI_OUT
      REAL VPHIG,CPHIG,DPHI,VTXTHE
      REAL ZTRPHI,VTXPHI,VX,VY,VZ,ZTRZ
      REAL ZTRX,ZTRY,MIN_DPHI_OLD
      REAL Z_VERT,Z_ERR,DEL_Z_VERT,ZV
      REAL FPHIG,FDCPHI,FDCZ,FDCX,FDCY
      REAL ZTR_PHI_ERR,ZTR_THE_ERR,ZTR_X_ERR,ZTR_Y_ERR
      REAL ZTR_Z_ERR,VTX_PHI_ERR,VTX_THE_ERR
      REAL VERT(3),VERT_ERR(3),DZ
      REAL ZTRTHE,FDCTHE,RADIUS
      REAL VPHI,CPHI,DPHIG,TOL_DZ
      REAL VERT_X,VERT_Y,VZ_TOL_CDC,VZ_TOL_FDC
      REAL DPHIG_TOL,DPHI_OLD_TOL,DPHI_TOL,ZTRA
      LOGICAL FIRST,DO_AREA,DO_W_GAIN,DO_E_GAIN
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
        CALL EZPICK('VTXCOFF_RCP')
        CALL EZGET('VERT_X',VERT_X,IER)
        CALL EZGET('VERT_Y',VERT_Y,IER)
        CALL EZGET('VZ_TOL_CDC',VZ_TOL_CDC,IER)
        CALL EZGET('VZ_TOL_FDC',VZ_TOL_FDC,IER)
        CALL EZGET('DPHIG_TOL',DPHIG_TOL,IER)
        CALL EZGET('DPHI_OLD_TOL',DPHI_OLD_TOL,IER)
        CALL EZGET('DPHI_TOL',DPHI_TOL,IER)
        CALL EZGET('AREA',DO_AREA,IER)
        CALL EZGET('W_GAIN',DO_W_GAIN,IER)
        CALL EZGET('E_GAIN',DO_E_GAIN,IER)
        CALL EZRSET
        FIRST = .FALSE.
      END IF
C
      N_VTX_TRAK = 0
      N_ZTRAK = 0
C
      EVENT = EVONUM()
C
C Get Vertex posision
C
      LVERT=GZVERT(1)
      IF (LVERT .EQ. 0 ) GOTO 2000      ! RETURN
      VERT(1) = VERT_X
      VERT(2) = VERT_Y
      VERT(3) = Q(LVERT+5)
      VERT_ERR(1) = 5.
      VERT_ERR(2) = 5.
      VERT_ERR(3) = Q(LVERT+8)
C
      LZTRH=GZZTRH()
      IF (LZTRH .EQ. 0) GOTO 2000
      N_ZTRAK = IQ(LZTRH+2)
C
C ****  Loop through VTXT banks and store info relevant for track matching
C
      CALL VZERO(NTRAK,3)
      LVTRH=GZVTRH()
      IF (LVTRH .EQ. 0) GOTO 2000       ! RETURN
      N_VTX_TRAK = IQ(LVTRH+2)          ! # OF TRACKS
      LVTXT = LQ(LVTRH-1)
      DO WHILE ( LVTXT .GT. 0 )
C
C ****  Only single layer tracks are used - check wire pattern to determine
C ****  layer
C
        IF ( IQ(LVTXT+3) .LE. 255 ) THEN
          LAYER = 0
        ELSEIF ( IQ(LVTXT+3) .LE. 65535 ) THEN
          LAYER = 1
        ELSE
          LAYER = 2
        ENDIF
        IF ( NTRAK(LAYER) .LT. MX_TRK_LAY ) THEN
          NTRAK(LAYER) = NTRAK(LAYER) + 1
          TRKPHI(NTRAK(LAYER),LAYER) = Q(LVTXT+6)
          TRKNUM(NTRAK(LAYER),LAYER) = IQ(LVTXT-5)
        ENDIF
        LVTXT = LQ(LVTXT)
      ENDDO
C
C ****  Sort the tracks in each layer in phi order
C
      DO LAYER = 0, 2
        IF ( NTRAK(LAYER) .GT. 0 )
     &    CALL SORTZV(TRKPHI(1,LAYER),INDEX(1,LAYER),NTRAK(LAYER),1,0,0)
      ENDDO
C
C Comparing ZTRACKS tracks with VTX tracks
C
      DO I_TRAK = 1,N_ZTRAK
        LZTRK=GZZTRK(I_TRAK)
        IF ( LZTRK .EQ. 0 ) GOTO 1000
        IF ( IQ(LZTRK+3) .NE. 0 ) THEN
          ID = 3                          ! MATCH WITH CDC TRACK
          TOL_DZ = VZ_TOL_CDC
        ELSE IF ( IQ(LZTRK+4) .NE. 0 ) THEN
          ID = 4                          ! MATCH WITH FDC TRACK
          TOL_DZ = VZ_TOL_FDC
        ELSE
          GOTO 1000
        END IF
        LZFIT = LQ(LZTRK-1)
        IF ( LZFIT .GT. 0 ) CALL MZDROP(IXCOM,LZFIT,' ')
        CALL ZTRKFT(LZTRK,VERT,VERT_ERR)
        LZTRK=GZZTRK(I_TRAK)
        LZFIT = LQ(LZTRK-1)
        IF ( Q(LZFIT+13) .EQ. 0. ) GOTO 1000    ! NO THETA AVAILABLE
        ZV = Q(LZFIT+15) - Q(LZFIT+14) / TAN(Q(LZFIT+13))
                                          ! Z POSITION OF TRACK AT X=Y=0
        DEL_Z_VERT = ABS(ZV - VERT(3))
        IF ( DEL_Z_VERT .GT. TOL_DZ ) GO TO 1000 ! Next ZTRK
        CPHI = Q(LZFIT+10)
        ZTRTHE = Q(LZFIT+13)
        ZTR_THE_ERR = Q(LZFIT+18)
        ZTRX = Q(LZFIT+11)
        ZTRY = Q(LZFIT+12)
        ZTRZ = Q(LZFIT+15)
        ZTR_Z_ERR = Q(LZFIT+19)*SIN(ZTRTHE)
        CPHIG = ATAN2(Q(LZFIT+12)-VERT(2),Q(LZFIT+11)-VERT(1))
        DO LAYER = 0,2
C
C ****  For each layer (for each ZTRK), find the closest track in phi, as well
C ****  as the next closest.
C
          MIN_DPHI = 10.
          K_TRAK = 1
          NEW_DPHI = ABS(TRKPHI(INDEX(K_TRAK,LAYER),LAYER)-CPHI)
          DO WHILE ( NEW_DPHI .LE. MIN_DPHI .AND. 
     &      K_TRAK .LE. NTRAK(LAYER) )
            IF ( NEW_DPHI .NE. MIN_DPHI ) THEN
              OLD_DPHI = MIN_DPHI
              MIN_DPHI = NEW_DPHI
            ENDIF
            K_TRAK = K_TRAK + 1
            IF ( K_TRAK .LE. NTRAK(LAYER) )
     &        NEW_DPHI = ABS(TRKPHI(INDEX(K_TRAK,LAYER),LAYER)-CPHI)
          ENDDO
          IF ( K_TRAK .EQ. NTRAK(LAYER) + 1 ) THEN  ! Handle branch cut
            PHI2PI = TRKPHI(INDEX(1,LAYER),LAYER) + TWOPI
            NEW_DPHI = ABS(PHI2PI-CPHI)
            IF ( NEW_DPHI .LT. MIN_DPHI ) THEN
              K_TRAK = 2  ! 1 higher than the real closest track
              OLD_DPHI = MIN_DPHI
              MIN_DPHI = NEW_DPHI
              PHI2PI = TRKPHI(INDEX(2,LAYER),LAYER) + TWOPI
              NEW_DPHI = ABS(PHI2PI-CPHI)
            ENDIF
          ENDIF
          IF ( MIN_DPHI .LT. DPHI_TOL ) THEN
            ADJACENT = MIN(NEW_DPHI,OLD_DPHI)
            IF ( ADJACENT .GT. DPHI_OLD_TOL ) THEN
              K_TRAK = K_TRAK - 1
              CLOSEST = TRKNUM(INDEX(K_TRAK,LAYER),LAYER)
              LVTXT = GZVTXT(CLOSEST)
              VPHIG = ATAN2(Q(LVTXT+8)-VERT(2),Q(LVTXT+7)-VERT(1))
              DPHIG = CPHIG-VPHIG
              IF ( ABS(DPHIG) .LT. DPHIG_TOL ) THEN
                IF ( DO_E_GAIN ) THEN
                  CALL VTXC_QDIV(EVENT,CLOSEST,ZTRTHE,ZTR_THE_ERR,
     &                  CPHI,ZTRX,ZTRY,ZTRZ,ZTR_Z_ERR,ID)
                END IF
                IF ( DO_AREA .OR. DO_W_GAIN ) THEN
                  CALL VTX_AREA_GAIN(CLOSEST,ZTRTHE,0)
                END IF
              END IF
            END IF
          END IF
        END DO
 1000 END DO
C
 2000 RETURN
      END
