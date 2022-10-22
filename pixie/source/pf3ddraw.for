      SUBROUTINE PF3DDRAW(IHALF,TRKNUM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw one specific FDC track with hits
C-                         in 3-D
C-
C-   Inputs  : IHALF  = FDC Half
C-             TRKNUM = FDC track number
C-   Outputs : Display on screen
C-
C-   Created  22-MAY-1990   Jeffrey Bantly
C-   Updated  23-JAN-1991   Jeffrey Bantly  add bank checks 
C-   Updated  20-FEB-1991   Lupe Howell  Converting to PIXIE using COMPACK 
C-   Updated  30-APR-1991   Jeffrey Bantly  make better use of Compack 
C-   Updated   1-JUL-1991   Robert E. Avery  Replace call to DRIDIR 
C-                                              with FDRIFTDIR.
C-   Updated  17-SEP-1991   Susan K. Blessing  Change size of (I)QTRAK to 
C-     26 (two errors and two spares).
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER HALF,UNIT,QUAD,SECTOR,WIRE,UB,LAYER,TRKNUM,IHALF
      INTEGER LEN,NTRK,IWORD,LADDER(0:2),LR
      INTEGER IHSEC,IHTRK(0:2),NHITS,PREV_WIRE,ILYR
      INTEGER IADD(34),IHIT(34),IWIRE(34)
      INTEGER IQTRAK(26),NEL,NWORDS
      INTEGER HALFL(0:2),UNITL(0:2),QUADL(0:2),SECTL(0:2)
      INTEGER SECTHI(34),BEGHIT,IADDRESS,FDC_HITS
      INTEGER DRHITS, DRSEGS, DRTRKS, DRSECS
      INTEGER LKFTRH,GZFTRH
      INTEGER IDX,LENGTH,IERR
C
      REAL QTRAK(26),CONT(26),QHSEC(3,34),RESID(34),DRIFTD(34)
      REAL QHIT(18),DELAY(0:2),DELHIT(0:2),STAGGER,FSTAGR
      REAL SDRIFT,CDRIFT,XC,YC,ZC,X(34),Y(34),Z(34),X0,Y0,Z0(2)
      REAL ZWB(0:2),ZWE(0:2),DXDZ,DYDZ,ZTRK,DIR,XTRK,YTRK
      REAL R,RPHI,WPHI,RPERP,TRKDELAY,RWIR,SECCOR
      EQUIVALENCE(QTRAK,IQTRAK)
C
      LOGICAL FIRST,IER,EZERROR
C
      CHARACTER*4 PATH,FPATH
      CHARACTER*4 HITCLR,TITCLR
      CHARACTER*112 FTEXT
C
      SAVE FIRST,PATH,Z0
      DATA HALFL,UNITL,QUADL,SECTL /12* -1/
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      IF(FIRST)THEN
        FIRST=.FALSE.
        CALL EZPICK('FTRAKS_RCP')
        IF( EZERROR(IER) ) THEN
          CALL ERRMSG('PIXIE','PF3DDRAW','Can not find FTRAKS_RCP','W')
        ELSE
          CALL EZGETS('FPATH',IDX,FPATH,LENGTH,IERR)
          CALL EZRSET
          PATH=FPATH
        ENDIF
      ENDIF
      CALL PATHST(PATH)
C
      LKFTRH=GZFTRH()
      IF(LKFTRH.GT.0) THEN
        Z0(1)=Q(LKFTRH+3)
        Z0(2)=Q(LKFTRH+4)
      ELSE
        CALL INTMSG(' No FDC track banks present')
        GOTO 999
      ENDIF
      CALL VZERO(DRIFTD,34)
      CALL VZERO(DELAY(0),3)
      CALL VZERO(CONT,26)
      CALL VZERO(QHSEC,102)
      CALL VFILL(HALFL(0),3,-1)
      CALL VFILL(UNITL(0),3,-1)
      CALL VFILL(QUADL(0),3,-1)
      CALL VFILL(SECTL(0),3,-1)
      CALL VFILL(IHTRK(0),3,-1)
C
C ****  Pick correct RCP bank
C
      CALL EZPICK('PX_FDCDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PF3DDRAW','Cannot find PX_FDCDIS_RCP','W')
        GOTO 999
      ENDIF
      CALL PUGETV('FDC DRAW 3DHITS',DRHITS)
      CALL PUGETV('FDC DRAW 3DSEGS',DRSEGS)
      CALL PUGETV('FDC DRAW 3DTRKS',DRTRKS)
      CALL PUGETV('FDC DRAW 3DSECS',DRSECS)
      CALL PUGETA('FDC COLR LABELS',TITCLR)
      CALL PUGETA('FDC COLR HITS',HITCLR)
      CALL GTFDCT(TRKNUM,CONT,QHSEC,LADDER)
      CALL UCOPY(CONT,QTRAK,26)
      CALL UCOPY_i(QHSEC(1,1),IADDRESS,1)
      IF (IQTRAK(2).EQ.0) THEN
        CALL INTMSG(' No hits on this track!!?????')
        GOTO 900
      ENDIF
      IF (IADDRESS.LE.0) THEN
        CALL INTMSG(' No hit info available for this track.')
        GOTO 900
      ENDIF
C
C      CALL J3MOVE(0.,0.,0.)
C      CALL AXIS
C
C  Label the plot with the parameters of track being shown.
C
      WRITE (FTEXT,101)
      CALL PF3MES(FTEXT)
      WRITE (FTEXT,102) TRKNUM,IQTRAK(2),
     X      (QTRAK(IWORD),IWORD=4,6),QTRAK(22),QTRAK(7),
     X      QTRAK(8),QTRAK(19)
      CALL PF3MES(FTEXT)
  101 FORMAT('  FDCT      NHIT    x0   ',
     X      '    y0       phi     theta    dx/dz    dy/dz  chisq ')
  102 FORMAT(1X,I4,4X,I6,6F9.3,E9.2)
C
C  Get information of hits on the track
C
      WIRE=-1
      NHITS=IQTRAK(2)
      DXDZ=QTRAK(7)
      DYDZ=QTRAK(8)
      DIR=-1.
      IF(HALF.EQ.1) DIR=1.
      CALL GTFDCH(FDC_HITS)
      DO 10 IHSEC=1,NHITS
        CALL UCOPY_i(QHSEC(1,IHSEC),IADD(IHSEC),1)
        CALL UCOPY_i(QHSEC(2,IHSEC),IHIT(IHSEC),1)
        CALL UCOPY(QHSEC(3,IHSEC),RESID(IHSEC),1)
        PREV_WIRE=WIRE
        CALL FCODER((IADD(IHSEC)/2),HALF,UNIT,QUAD,SECTOR,WIRE,UB,1)
        LAYER=UNIT*2 + QUAD/4
        SECTHI(IHSEC)=SECTOR
        IWIRE(IHSEC)=WIRE
        IF(IHSEC.EQ.1 .AND. LAYER.EQ.0) THEN
          HALFL(0)=HALF
          UNITL(0)=0
          QUADL(0)=QUAD
          SECTL(0)=SECTOR
        ENDIF
C
        IF(UNIT.EQ.0) THEN
          IF(QUAD.EQ.QUADL(0)) THEN
            IHTRK(0)=IHSEC
            IHTRK(1)=IHSEC
            IHTRK(2)=IHSEC
          ELSE
            HALFL(1)=HALF
            UNITL(1)=0
            QUADL(1)=QUAD
            SECTL(1)=SECTOR
            IHTRK(1)=IHSEC
            IHTRK(2)=IHSEC
          ENDIF
        ELSEIF(UNIT.EQ.1) THEN
          HALFL(2)=HALF
          UNITL(2)=1
          QUADL(2)=0
          SECTL(2)=SECTOR
          IHTRK(2)=IHSEC
        ELSE
          CALL INTMSG(' Bad value for Unit in hit!!??')
          GOTO 10
        ENDIF
        IF(FDC_HITS .LE.0 ) GOTO 10
        IF (UNIT.LE.0) THEN
          CALL GTFTSC(HALF,QUAD,SECTOR,'HIT',IHIT(IHSEC),NEL,
     X           NWORDS,QHIT)
        ELSE
          CALL GTFPSC(HALF,SECTOR,'HIT',IHIT(IHSEC),NEL,NWORDS,QHIT)
        END IF
        IF(WIRE.EQ.PREV_WIRE .AND. WIRE.EQ.0) THEN
          DELAY(LAYER)=QHIT(4)
          DELHIT(LAYER)=IHSEC
        ELSE
          LR=IADD(IHSEC)-2*(IADD(IHSEC)/2)
          STAGGER=0.
          STAGGER=FSTAGR(HALF,UNIT,QUAD,SECTOR,WIRE)
          DRIFTD(IHSEC)=QHIT(2+LR)-STAGGER
        ENDIF
   10 CONTINUE
C
C  Draw relevant sector shapes and hits
C
      CALL JCMARK(2)
      CALL JSIZE(0.005,0.005)
      DO 20 ILYR=0,2
        IF(HALFL(ILYR).GE.0) THEN
          IF(DRSECS.EQ.1) THEN    ! Draw in relevant sector shape
            IF(ILYR.LE.1) THEN
              CALL PF3DTH(HALFL(ILYR),QUADL(ILYR),SECTL(ILYR))
            ELSE
              CALL PF3DPH(HALFL(ILYR),SECTL(ILYR))
            ENDIF
          ENDIF
          IF(DRHITS.EQ.1 .AND. FDC_HITS.GT.0) THEN
            BEGHIT=1
            IF(ILYR.GT.0) BEGHIT=IHTRK(ILYR-1)+1
            IF(BEGHIT.LT.1) BEGHIT=1
            CALL PXCOLR(HITCLR)
            DO 30 IHSEC=BEGHIT,IHTRK(ILYR)
              IF(IHSEC.EQ.DELHIT(ILYR)) GOTO 30
              CALL FDRIFTDIR(
     &          HALFL(ILYR),UNITL(ILYR),QUADL(ILYR),
     &          SECTHI(IHSEC),IWIRE(IHSEC),
     &          SDRIFT,CDRIFT)
              CALL GTFALH(HALFL(ILYR),UNITL(ILYR),QUADL(ILYR),
     &           SECTHI(IHSEC),IWIRE(IHSEC),XC,YC,ZC)
              IF(ZC.EQ. 0.0 .OR. ABS(ZC).GT. 150.) THEN
                CALL INTMSG(' Bad value for ZC for wire!!??')
                GOTO 30
              ENDIF
              IF(IHSEC.EQ.BEGHIT) ZWB(ILYR)=ZC
              IF(IHSEC.EQ.IHTRK(ILYR)) ZWE(ILYR)=ZC
              IF(FDC_HITS.LE.0) GOTO 30
              XTRK=QTRAK(4)+DXDZ*(ZC-Z0(HALF+1))
              YTRK=QTRAK(5)+DYDZ*(ZC-Z0(HALF+1))
              R=SQRT(XTRK**2 + YTRK**2)
              IF(XTRK.EQ.0.0) THEN
                XTRK = 0.00000001
                IF(YTRK.GE. 0.0) RPHI=PI/2.
                IF(YTRK.LT. 0.0) RPHI=3.*PI/2.
              ELSE
                RPHI=ATAN2(YTRK,XTRK)
              ENDIF
              IF(XC.EQ. 0.0) THEN
                XC = 0.00000001
                IF(YC.GE. 0.0) WPHI=PI/2.
                IF(YC.LT. 0.0) WPHI=3.*PI/2.
              ELSE
                WPHI=ATAN2(YC,XC)
              ENDIF
              RWIR=R*COS(RPHI-WPHI)
              RPERP=R*SIN(RPHI-WPHI)
              SECCOR=1.0
              IF(ILYR.LE.1 .AND. SECTHI(IHSEC).EQ.1) SECCOR=-1.0
              IF(ILYR.LE.1) THEN
                IF(IHSEC.EQ.BEGHIT) TRKDELAY=RPERP
C                IF(DELAY(ILYR).EQ.0.0) THEN
                  X(IHSEC)=XC+DRIFTD(IHSEC)*CDRIFT+
     &              (RPERP)*(-SDRIFT)*SECCOR
                  Y(IHSEC)=YC+DRIFTD(IHSEC)*SDRIFT+
     &              (RPERP)*CDRIFT*SECCOR
C                ELSE
C                  X(IHSEC)=XC+DRIFTD(IHSEC)*CDRIFT+
C     &             (DELAY(ILYR)+(RPERP-TRKDELAY))*(-SDRIFT)*SECCOR
C                  Y(IHSEC)=YC+DRIFTD(IHSEC)*SDRIFT+
C     &             (DELAY(ILYR)+(RPERP-TRKDELAY))*CDRIFT*SECCOR
C                ENDIF
                Z(IHSEC)=ZC
              ELSE
                X(IHSEC)=DRIFTD(IHSEC)*CDRIFT+
     &             RWIR*(SDRIFT)
                Y(IHSEC)=DRIFTD(IHSEC)*SDRIFT+
     &             RWIR*(-CDRIFT)
                Z(IHSEC)=ZC
              ENDIF
              CALL J3MOVE(X(IHSEC),Y(IHSEC),Z(IHSEC))
              CALL J3MARK(X(IHSEC),Y(IHSEC),Z(IHSEC))
   30       CONTINUE
          ENDIF
        ENDIF
   20 CONTINUE
C
C  Draw track up to chamber and sections of the track through sectors.
C
      CALL PXCOLR(TITCLR)
      CALL J3MOVE(0.,0.,0.)
      CALL J3DRAW(10.,0.,0.)
      CALL J3MOVE(0.,0.,0.)
      CALL J3DRAW(0.,10.,0.)
      CALL J3MOVE(QTRAK(4),QTRAK(5),Z0(HALF+1))
      CALL JR3MOV(DXDZ*(-5.*DIR),DYDZ*(-5.*DIR),(-5.*DIR))
      X0=QTRAK(4)-DXDZ*(Z0(HALF+1)-5.*DIR)
      Y0=QTRAK(5)-DYDZ*(Z0(HALF+1)-5.*DIR)
      CALL J3DRAW(X0,Y0,0.)
      CALL J3DRAW(X0+10.,Y0,0.)
      CALL J3MOVE(X0,Y0,0.)
      CALL J3DRAW(X0,Y0+10.,0.)
      IF (DRHITS.LT.1) GOTO 900
      DO 40 ILYR=0,2
        XTRK=QTRAK(4)+DXDZ*(ZWB(ILYR)-Z0(HALF+1))
        YTRK=QTRAK(5)+DYDZ*(ZWB(ILYR)-Z0(HALF+1))
        CALL J3MOVE(XTRK,YTRK,ZWB(ILYR))
        CALL JR3DRA(DXDZ*(ZWE(ILYR)-ZWB(ILYR)),
     &          DYDZ*(ZWE(ILYR)-ZWB(ILYR)),(ZWE(ILYR)-ZWB(ILYR)))
   40 CONTINUE
C
C  Done.
C
C----------------------------------------------------------------------
C
C ****  Reset RCP bank
C
  900 CONTINUE
      CALL EZRSET
  999 CONTINUE
      CALL PATHRS()
      RETURN
      END
