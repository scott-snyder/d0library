      SUBROUTINE PF3DSGDR(MODULE,SEGNUM,TRKRAD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw the FDC sector box and hits for a given
C-                         segment.
C-
C-   Inputs  : MODULE = FDC Module number
C-             SEGNUM = Segment number in that module
C-   Outputs : draws box and hits
C-
C-   Created  21-JUN-1990   Jeffrey Bantly
C-   Updated   5-APR-1991   Jeffrey Bantly  general cleanup,remove CYLTRK 
C-   Updated  30-APR-1991   Jeffrey Bantly  make better use of Compack 
C-   Updated  14-MAY-1991   Susan K. Blessing  Use North and South halves 
C-   Updated  17-JUN-1991   Susan K. Blessing  Change size of CONT array.
C-   Updated   1-JUL-1991   Robert E. Avery  Replace call to DRIDIR 
C-                                              with FDRIFTDIR.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      INTEGER SEGNUM,ISECT,IADD,NHITS,HALF,UNIT,QUAD,SECTOR
      INTEGER NSEG,NZBANK,LSEG,MODULE,LAYER,WIRE,UB,IHITD,IDIR
      INTEGER IHIT,IWIRE(NBPSEN),LR,IPTR,GZFSEG,ICALL,IOFSET
      INTEGER DRHITS, DRSEGS, DRTRKS, DRSECS,NEL,NWORDS,DELSECT
      INTEGER IDX,LENGTH,IER
C
      REAL CONT(62),PHI,THETA,DR1,Z1,FISECT,FIADD,FNHITS,RESID
      REAL TRKRAD,DRIFTD(NBPSEN),RPERP,RWIR,TRKDELAY
      REAL SDRIFT,CDRIFT,STAGGER,FSTAGR,QHIT(18),DELAY,SECCOR
      REAL X(NBPSEN),Y(NBPSEN),Z(NBPSEN),XC,YC,ZC
      EQUIVALENCE(ISECT,FISECT)
      EQUIVALENCE(IADD,FIADD)
      EQUIVALENCE(NHITS,FNHITS)
C
      CHARACTER*1 CHALF(0:1),CUNIT(0:1)
      CHARACTER*4 PATH,FPATH
      CHARACTER*4 HITCLR
      CHARACTER*112 FTEXT
C
      LOGICAL EZERROR
C
      DATA CHALF/'N','S'/
      DATA CUNIT/'T','P'/
      DATA ICALL/0/
C----------------------------------------------------------------------
      IF( ICALL.EQ.0 ) THEN
        ICALL=1
        CALL EZPICK('FTRAKS_RCP')
        IF( EZERROR(IER) ) THEN
          CALL ERRMSG('PIXIE','PF3DSGDR','Can not find FTRAKS_RCP','W')
        ELSE
          CALL EZGETS('FPATH',IDX,FPATH,LENGTH,IER)
          CALL EZRSET
          PATH=FPATH
        ENDIF
      ENDIF
      CALL PATHST(PATH)
C
C ****  Pick correct RCP bank
C
      CALL EZPICK('PX_FDCDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PF3DSGDR','Cannot find PX_FDCDIS_RCP','W')
        GOTO 999
      ENDIF
      CALL PUGETV('FDC DRAW 3DHITS',DRHITS)
      CALL PUGETV('FDC DRAW 3DSEGS',DRSEGS)
      CALL PUGETV('FDC DRAW 3DTRKS',DRTRKS)
      CALL PUGETV('FDC DRAW 3DSECS',DRSECS)
      CALL PUGETA('FDC COLR HITS',HITCLR)
      CALL EZRSET
      HALF=MODULE/3
      LAYER=MODULE - HALF*3
      LSEG=GZFSEG(HALF,LAYER)
      IF(LSEG.LE.0) THEN
        GOTO 999
      ENDIF
      NSEG=NZBANK(0,LSEG)
      IF(SEGNUM.GT.NSEG) THEN
        GOTO 999
      ENDIF
      CALL GTFSEG(MODULE,SEGNUM,CONT)
      FISECT=CONT(1)
      FIADD=CONT(2)
      FNHITS=CONT(3)
      IF (NHITS.LE.0) THEN
        GOTO 999
      ENDIF
      CALL FCODER(IADD,HALF,UNIT,QUAD,SECTOR,WIRE,UB,1)
      IHITD=20
      DELSECT=0
      IF(ISECT.NE.SECTOR) THEN
        IHITD=ABS(ISECT/1000)
        DELSECT=ISECT/ABS(ISECT)
      ENDIF
C
C  Label the plot with the parameters of segment being shown.
C
      CALL PXCOLR('BLU')
      IF(UNIT.EQ.0) THEN
        PHI=CONT(20)
        THETA=CONT(21)
        WRITE (FTEXT,101)
        CALL PF3MES(FTEXT)
        WRITE( FTEXT,102) SEGNUM,CHALF(HALF),CUNIT(UNIT),QUAD,SECTOR,
     &    NHITS,PHI,(PHI*360./TWOPI),THETA,(THETA*360./TWOPI)
        CALL PF3MES(FTEXT)
      ELSE
        PHI=CONT(36)
        DR1=CONT(37)
        Z1=CONT(38)
        WRITE (FTEXT,105)
        CALL PF3MES(FTEXT)
        WRITE( FTEXT,106) SEGNUM,CHALF(HALF),CUNIT(UNIT),SECTOR,
     &    NHITS,PHI,(PHI*360./TWOPI),DR1,Z1
        CALL PF3MES(FTEXT)
      ENDIF
C
C  Draw sector shape in display.
C
      IF(DRSECS.EQ.1) THEN    ! Draw in relevant sector shape
        IF(UNIT.EQ.0) CALL PF3DTH(HALF,QUAD,SECTOR)
        IF(UNIT.EQ.1) CALL PF3DPH(HALF,SECTOR)
      ENDIF
C
C  Draw hits on segment in display.
C
      IDIR=0
      IF(UNIT.EQ.0) IOFSET=8
      IF(UNIT.EQ.1) IOFSET=16
      DO 200 IHIT=1, NHITS
        IWIRE(IHIT)=INT(CONT(3+IHIT)/2.)
        LR   =INT(CONT(3+IHIT))-IWIRE(IHIT)*2
        IPTR =INT(CONT(3+IHIT+IOFSET))
        RESID=CONT(3+IHIT+2*(IOFSET+1)+UNIT)
        IF(IHIT.GE.IHITD) IDIR=DELSECT
        IF (UNIT.LE.0) THEN
          CALL GTFTSC(HALF,QUAD,SECTOR+IDIR,'HIT',IPTR,NEL,
     X           NWORDS,QHIT)
        ELSE
          CALL GTFPSC(HALF,SECTOR+IDIR,'HIT',IPTR,NEL,NWORDS,QHIT)
        END IF
        IF(IWIRE(IHIT).EQ.0) DELAY=QHIT(4)
        STAGGER=0.
        STAGGER=FSTAGR(HALF,UNIT,QUAD,SECTOR+IDIR,IWIRE(IHIT))
        DRIFTD(IHIT)=QHIT(2+LR)-STAGGER
  200 CONTINUE
C
C  Draw relevant sector shapes and hits
C
      CALL JCMARK(2)
      CALL JSIZE(0.005,0.005)
      CALL FDRIFTDIR(HALF,UNIT,QUAD,SECTOR,WIRE,SDRIFT,CDRIFT)
      CALL PXCOLR(HITCLR)
      IDIR=0
      DO 30 IHIT=1,NHITS
        IF(IHIT.GE. IHITD) IDIR=DELSECT
        CALL GTFALH(HALF,UNIT,QUAD,SECTOR+IDIR,IWIRE(IHIT),XC,YC,ZC)
        IF(ZC.EQ. 0.0) GOTO 900
        RWIR=TRKRAD
        RPERP=0.0
        IF(UNIT.EQ.0) THEN
          SECCOR=1.0
          IF( (SECTOR+IDIR) .EQ. 1 ) SECCOR = -1.0
          IF(IHIT.EQ.1) TRKDELAY=RPERP
          X(IHIT)=XC+DRIFTD(IHIT)*CDRIFT+
     &             (DELAY+(RPERP-TRKDELAY))*(-SDRIFT)*SECCOR
          Y(IHIT)=YC+DRIFTD(IHIT)*SDRIFT+
     &             (DELAY+(RPERP-TRKDELAY))*CDRIFT*SECCOR
          Z(IHIT)=ZC
        ELSE
          X(IHIT)=DRIFTD(IHIT)*CDRIFT+
     &             RWIR*(SDRIFT)
          Y(IHIT)=DRIFTD(IHIT)*SDRIFT+
     &             RWIR*(-CDRIFT)
          Z(IHIT)=ZC
        ENDIF
        IF(DRHITS.EQ.1) THEN
          CALL J3MOVE(X(IHIT),Y(IHIT),Z(IHIT))
          CALL J3MARK(X(IHIT),Y(IHIT),Z(IHIT))
        ENDIF
   30 CONTINUE
C
C  Done.
C
      GOTO 999
C
C  Error condition messages and handling.
C
  900 CONTINUE
      CALL OUTMSG(' Bad value for ZC for wire!!??')
      GOTO 999
C
  101 FORMAT('  FSG     HALF UNIT QUAD SECT  NHITS   PHI (DEG)',
     &         '    THETA (DEG)')
  102 FORMAT(I5,3X,2A5,2I5,2X,I4,2X,F6.2,F7.1,2X,F6.2,F7.1)
  105 FORMAT('  FSG     HALF UNIT SECT  NHITS   PHI  (DEG)   DR1',
     X'    Z1')
  106 FORMAT(I5,3X,2A5,I5,2X,I5,X,F6.2,F6.1,2F8.2)
C--------------------------------------------------------------------------
  999 CONTINUE
      CALL PATHRS()
      RETURN
      END
