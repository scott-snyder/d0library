      SUBROUTINE PFHITS_3D
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Mark the locations of the FDC delay line hits
C-                              in 3d.
C-   
C-   Inputs  : NONE
C-   Outputs : NONE
C-
C-   Created  23-JUN-1992   Robert E. Avery (from PFDHDR)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$INC:FDDELP.INC'    ! will contain delay line hit info
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER H,U,QU,S,IHIT
      INTEGER LR,DRAW_HITS
      INTEGER IER
C
      REAL    XDRIFT,YDRIFT
      REAL    XC,YC,ZC,ANG,XMIN,XMAX,DIR,SECCOR
      REAL    DX,DY
C
      REAL    CSIZE
      PARAMETER( CSIZE = 0.3 )         ! Symbol size in CM
C
      CHARACTER*4 HITCLR
      CHARACTER*4 TKHITCLR
C----------------------------------------------------------------------
      CALL EZPICK('PX_FDCDIS_RCP')
      CALL PUGETA('FDC COLR HITS',HITCLR)
      CALL PUGETA('FDC COLR TKHITS',TKHITCLR)
      CALL PUGETV('FDC DRAW 3DHITS',DRAW_HITS)
      CALL EZRSET
      IF ( DRAW_HITS.LT.1 ) GOTO 999
C
      CALL PFGETD                            ! Find delay line hits
C
C  Loop over all sense wire 0 hits in FDC.
C
      CALL PUOPEN
      DO H=0,1
        U=0
        DO QU=0,7
          DIR = 1.
          IF( QU.EQ.2 .OR. QU.EQ.3 ) DIR = -1.
          IF( QU.EQ.6 .OR. QU.EQ.7 ) DIR = -1.
          DO S=0,5
            DO IHIT=1,N_DL_HITS(H,QU,S)
              CALL GTFALH(H,U,QU,S,0,XC,YC,ZC)
              ANG=0.
              IF( QU.LE.3 ) ANG=PI/4.
              SECCOR=1.
              IF( S .EQ. 1 ) SECCOR=-1.
C
C   Calculate position displacement from wire center.
C
              IF( QU.LE.3 ) THEN
                IF( QU .EQ. 1 .OR. QU .EQ. 3 ) THEN
                  DY=DIR*(DL_DIST(H,QU,S,IHIT)*SIN(ANG)*(-1.) +
     &                SECCOR*DRIFT_DIST(H,QU,S,IHIT)*COS(ANG))
                  DX=DIR*(-1.)*(DL_DIST(H,QU,S,IHIT)*COS(ANG) -
     &                SECCOR*(-1.)*DRIFT_DIST(H,QU,S,IHIT)*SIN(ANG))
                ELSE
                  DY=DIR*(DL_DIST(H,QU,S,IHIT)*COS(ANG) -
     &                SECCOR*(-1.)*DRIFT_DIST(H,QU,S,IHIT)*SIN(ANG))
                  DX=DIR*(-1.)*(DL_DIST(H,QU,S,IHIT)*SIN(ANG) +
     &                SECCOR*(-1.)*DRIFT_DIST(H,QU,S,IHIT)*COS(ANG))
                ENDIF
              ELSE
                IF( QU .EQ. 5 .OR. QU .EQ. 7 ) THEN
                  DX=DL_DIST(H,QU,S,IHIT)*DIR*(-1.)
                  DY=SECCOR*DRIFT_DIST(H,QU,S,IHIT)*DIR
                ELSE
                  DX=SECCOR*DRIFT_DIST(H,QU,S,IHIT)*DIR
                  DY=DL_DIST(H,QU,S,IHIT)*DIR
                ENDIF
              ENDIF
C
C  Calculate hit location in x,y.
C
              XDRIFT=XC+DX
              YDRIFT=YC+DY
C
C  Plot delay line assocaited sense wire 0 hits.
C
              IF ( ON_TRK(H,QU,S,IHIT) ) THEN
                CALL PXCOLR(TKHITCLR)
                CALL PZDRAW_3DHIT(XDRIFT,YDRIFT,ZC,CSIZE)
              ELSE
                CALL PXCOLR(HITCLR)
                CALL PZDRAW_3DHIT(XDRIFT,YDRIFT,ZC,CSIZE)
              ENDIF
C
            ENDDO                       ! End loop over FDC Hits on SW0   
          ENDDO                         ! End loop over FDC Sector.       
        ENDDO                           ! End loop over FDC Quad.         
      ENDDO                             ! End loop over FDC Half.         
      CALL JRCLOS
C----------------------------------------------------------------------
  999 RETURN
      END
