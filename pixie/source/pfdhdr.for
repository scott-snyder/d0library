      SUBROUTINE PFDHDR(PHI1,PHI2,PHI3,PHI4,IVIEW)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Mark the locations of the delay line hits
C-
C-   Inputs  : PHI1,PHI2,PHI3,PHI4 = Phi limits of plot
C-   Outputs : Marks delay line hits
C-   Controls: IVIEW =  1=XZ,2=YZ,3=RZ,4=XY view drawn
C-
C-   Created  27-FEB-1989   Jeffrey Bantly
C-   Updated  10-JAN-1990   Lupe Howell Implementing Color Table
C-   Updated   7-FEB-1990   Jeffrey Bantly  cleanup temp fixes
C-   Updated   5-NOV-1990   Jeffrey Bantly  add phi angle limits 
C-   Updated  22-JAN-1991   Jeffrey Bantly  convert PFDHRZ to this
C-                                          all-view routine 
C-   Updated  30-APR-1991   Jeffrey Bantly  make better use of Compack 
C-   Updated  27-AUG-1991   Robert E. Avery  Don't  draw hit if no DL position.
C-   Updated   7-OCT-1991   Robert E. Avery  Use new FDDELP 
C-   Updated   8-NOV-1991   Robert E. Avery  Check phi bounds with call to
C-                              new function, PFPHICHK.
C-   Updated  21-NOV-1991   Robert E. Avery  Upper half of plot is PHI1-PHI2 
C-   Updated  30-MAR-1992   Robert E. Avery  Change colors, use different
C-      color for hits on tracks.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$INC:FDDELP.INC'    ! will contain delay line hit info
      INCLUDE 'D0$INC:PXPARA.INC'
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER IVIEW
      INTEGER H,U,QU,S,IHIT
      INTEGER LR
      INTEGER IER
      INTEGER PFPHICHK,RSIGN
C
      REAL    XDRIFT,YDRIFT
      REAL    RDRIFT,RDRFT2,PHI1,PHI2,PHI3,PHI4
      REAL    XC,YC,ZC,ANG,XMIN,XMAX,DIR,SECCOR
      REAL    DX,DXP,DY,DYP,YXRATI
      REAL    CSIZE
      PARAMETER( CSIZE = 3.0 )         ! Symbol size in CM
      CHARACTER*4 HITCLR
      CHARACTER*4 TKHITCLR
C----------------------------------------------------------------------
      CALL EZPICK('PX_FDCDIS_RCP')
      CALL PUGETA('FDC COLR HITS',HITCLR)
      CALL PUGETA('FDC COLR TKHITS',TKHITCLR)
      CALL EZRSET
C
C  Make sure labels are centered on point
C
      YXRATI = YDVMAG/XDVMAG  
      CALL JSIZE(YXRATI*CSIZE,CSIZE)
      CALL JJUST(2,2)
C
C  Loop over all sense wire 0 hits in FDC.
C
      DO 10 H=0,1
        U=0
        DO 30 QU=0,7
          DIR = 1.
          IF( QU.EQ.2 .OR. QU.EQ.3 ) DIR = -1.
          IF( QU.EQ.6 .OR. QU.EQ.7 ) DIR = -1.
          DO 40 S=0,5
            DO 50 IHIT=1,N_DL_HITS(H,QU,S)
              CALL GTFALH(H,U,QU,S,0,XC,YC,ZC)
              IF( ZC.EQ. 0.0) GOTO 40
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
C  Check for phi angle and determine if it is within bounds.
C
              RSIGN = PFPHICHK(XDRIFT,YDRIFT,PHI1,PHI2,PHI3,PHI4) 
              IF ( RSIGN .EQ. 0 ) THEN
                GOTO 50
              ENDIF
C
C  Plot delay line assocaited sense wire 0 hits.
C
              RDRIFT = RSIGN * SQRT(XDRIFT**2 + YDRIFT**2)
C
              IF(IVIEW.EQ.1) THEN
                CALL JMOVE(ZC,XDRIFT)
C
              ELSEIF(IVIEW.EQ.2) THEN
                CALL JMOVE(ZC,YDRIFT)
C
              ELSEIF(IVIEW.EQ.3) THEN
                CALL JMOVE(ZC,RDRIFT)
C
              ELSEIF(IVIEW.EQ.4) THEN
                CALL JMOVE(XDRIFT,YDRIFT)
C
              ENDIF
              IF ( ON_TRK(H,QU,S,IHIT) ) THEN
                CALL PXCOLR(TKHITCLR)
                CALL J3STRG('*')
              ELSE
                CALL PXCOLR(HITCLR)
                CALL J3STRG('+')
              ENDIF
C
   50       CONTINUE                    ! End loop over FDC Hits on SW0
   40     CONTINUE                      ! End loop over FDC Sector.
   30   CONTINUE                        ! End loop over FDC Quad.
   10 CONTINUE                          ! End loop over FDC Half.
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
