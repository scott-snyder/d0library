      SUBROUTINE PFDELH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Mark the locations of the delay line hits
C-
C-   Inputs  : none
C-   Outputs : Marks delay line hits
C-
C-   Created  27-FEB-1989   Jeffrey Bantly
C-   Updated  10-JAN-1990   Lupe Howell Implementing Color Table
C-   Updated   7-FEB-1990   Jeffrey Bantly  cleanup temp fixes
C-   Updated  13-JUN-1990   Jeffrey Bantly  remove passed parameters
C-   Updated   2-JAN-1991   Jeffrey Bantly  re-arrange init for speedup 
C-   Updated  20-FEB-1991   Lupe Howell  Implementing PIXIE using COMPACK 
C-   Updated   6-AUG-1991   Robert E. Avery Only display correct side hit,
C-                              fix hit positioning problems.
C-   Updated  27-AUG-1991   Robert E. Avery  Don't  draw hit if no DL position.
C-   Updated   7-OCT-1991   Robert E. Avery  Use new FDDELP 
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
      INTEGER H,QU,S,IHIT
      INTEGER ICALL,IER
      INTEGER LR
      REAL    XDRIFT,YDRIFT
      REAL    XC,YC,ZC,ANG,CANG,SANG,DIR,SECCOR
      REAL    DX,DY,YXRATI
      LOGICAL EZERROR
      REAL    CSIZE
      PARAMETER( CSIZE = 3.0 )         ! Symbol size in CM
      CHARACTER*4 HITCLR
      CHARACTER*4 TKHITCLR
C----------------------------------------------------------------------
C
C ****  Pick correct RCP bank
C
      CALL EZPICK('PX_FDCDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PFDELH','Cannot find PX_FDCDIS_RCP','W')
        GOTO 999
      ENDIF
      CALL PUGETV('FDC HALF',H)
      CALL PUGETV('FDC QUAD',QU)
      CALL PUGETV('FDC SECT',S)
      CALL PUGETA('FDC COLR HITS',HITCLR)
      CALL PUGETA('FDC COLR TKHITS',TKHITCLR)
      CALL EZRSET
      IF( N_DL_HITS(H,QU,S) .LT. 1 ) GOTO 999
C
C  Make sure labels are centered on point
C
      YXRATI = YDVMAG/XDVMAG  
      CALL JSIZE(YXRATI*CSIZE,CSIZE)
      CALL JJUST(2,2)
C
      DIR=1.
      IF(QU.EQ.2 .OR. QU.EQ.3) DIR=-1.
      IF(QU.EQ.6 .OR. QU.EQ.7) DIR=-1.
      CALL GTFALH(H,0,QU,S,0,XC,YC,ZC)
      IF(ZC.EQ. 0.0) GOTO 999
      ANG=0.
      IF( QU.LE.3 ) ANG=PI/4.
      SECCOR=1.
      IF( S .EQ. 1 ) SECCOR=-1.
      CANG=COS(ANG)
      SANG=SIN(ANG)
C
      DO 10 IHIT=1,N_DL_HITS(H,QU,S)
        IF( QU.LE.3 ) THEN
          IF( QU .EQ. 1 .OR. QU .EQ. 3 ) THEN
            DY=(DL_DIST(H,QU,S,IHIT)*SANG*(-1.) +
     &             SECCOR*DRIFT_DIST(H,QU,S,IHIT)*CANG)*DIR   !
            DX=(DL_DIST(H,QU,S,IHIT)*CANG -
     &           SECCOR*(-1.)*DRIFT_DIST(H,QU,S,IHIT)*SANG)*(-1.)*DIR
          ELSE
            DY=(DL_DIST(H,QU,S,IHIT)*CANG -
     &           SECCOR*(-1.)*DRIFT_DIST(H,QU,S,IHIT)*SANG)*DIR
            DX=(DL_DIST(H,QU,S,IHIT)*SANG +
     &           SECCOR*(-1.)*DRIFT_DIST(H,QU,S,IHIT)*CANG)*DIR*(-1.)
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
        XDRIFT=XC+DX
        YDRIFT=YC+DY
C
        CALL JMOVE(XDRIFT,YDRIFT)
        IF ( ON_TRK(H,QU,S,IHIT) ) THEN
          CALL PXCOLR(TKHITCLR)
          CALL J3STRG('*')
        ELSE
          CALL PXCOLR(HITCLR)
          CALL J3STRG('+')
        ENDIF
C
   10 CONTINUE
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
