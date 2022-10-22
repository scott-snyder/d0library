      SUBROUTINE PF3DSEGS(HALF,LADDER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw sectors and hits and possibly a track
C-                         along a specific FDC segment ladder in 3-D
C-
C-   Inputs  : HALF
C-             LADDER
C-   Outputs : Display on screen
C-
C-   Created  22-MAY-1990   Jeffrey Bantly
C-   Updated   2-JAN-1991   Susan K. Blessing  Change FTFDCT call 
C-   Updated  21-JAN-1991   Jeffrey Bantly  add bank link checks, Z0 change 
C-   Updated  30-APR-1991   Jeffrey Bantly  make better use of Compack 
C-   Updated  14-MAY-1991   Susan K. Blessing  Use North and South halves 
C-   Updated  17-FEB-1992   Robert E. Avery  Compute average radius for PHI 
C-       here rather than in pfpick_segmt (eliminate X02, Y03 from RCP)
C-   Updated  16-MAR-1992   Robert E. Avery    Change FTFDCT call  
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER HALF,LADDER(0:2)
      INTEGER LEN,NTRK,IWORD
      INTEGER IHSEC,IHTRK(0:2),ICALL,NHITS,ILYR,IMOD
      INTEGER IB0,IB1,IB2,IB3
      INTEGER DRHITS, DRSEGS, DRTRKS, DRSECS
      INTEGER NTRACK,ITRACK,IER,IVIEW
      INTEGER LKFTRH,GZFTRH
      INTEGER IADD
      INTEGER NLYR
      INTEGER H,UNIT,QUAD,SECTOR,WIRE,UB
C
      REAL X2SUM,Y2SUM
      REAL CHINORM,CHIMAX
      REAL QHIT(18),X0,Y0,Z0(2),TRKRAD,X(3),Y(3),Z(3)
      REAL ZWB(0:2),ZWE(0:2),DXDZ,DYDZ,DIR,XTRK,YTRK,X02,Y02
      REAL A(0:2),B(0:2),C(0:2),D(0:2),X1,Y1,Z1,X2,Y2,Z2
      REAL XC,YC,ZC
C
      INTEGER IQTRAK(22),IQHSEC(3,34)
      REAL QTRAK(22),QHSEC(3,34)
      EQUIVALENCE(QTRAK,IQTRAK)
      EQUIVALENCE(QHSEC,IQHSEC)
C
      INTEGER ICONT(62)
      REAL    CONT(62)
      EQUIVALENCE(CONT,ICONT)
C
      CHARACTER*4 HITCLR,TITCLR
      CHARACTER*112 FTEXT
C
      LOGICAL OK
      LOGICAL FCHEKL
C
      SAVE ICALL,CHIMAX
      DATA ICALL/0/
C----------------------------------------------------------------------
      IF (ICALL.EQ.0) THEN
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('CHIMAX',CHIMAX,IER)
        CALL EZRSET
        ICALL=1
      ENDIF
C
      LKFTRH=GZFTRH()
      IF (LKFTRH.GT.0) THEN
        Z0(1)=Q(LKFTRH+3)
        Z0(2)=Q(LKFTRH+4)
      ELSE
        CALL OUTMSG(' No FDC track banks present')
        GOTO 999
      ENDIF
C
      CALL VZERO(QHSEC,102)
      CALL VZERO(A(0),3)
      CALL VZERO(B(0),3)
      CALL VZERO(C(0),3)
      CALL VZERO(D(0),3)
      CALL PUGETV('FDC DRAW 3DHITS',DRHITS)
      CALL PUGETV('FDC DRAW 3DSEGS',DRSEGS)
      CALL PUGETV('FDC DRAW 3DTRKS',DRTRKS)
      CALL PUGETV('FDC DRAW 3DSECS',DRSECS)
      CALL PUGETA('FDC COLR LABELS',TITCLR)
      CALL PUGETA('FDC COLR HITS',HITCLR)
      CALL PUGETV('FDC 3D VIEW',IVIEW)
      CALL J3MOVE(0.,0.,0.)
      CALL AXIS
C
C  Draw the FDC segments as chosen.
C
      X2SUM=0.0
      Y2SUM=0.0
      NLYR=0
      DO 10 ILYR=0,2
        IF (LADDER(ILYR).LE.0) GOTO 10
        IMOD=HALF*3+ILYR
        IF ( ILYR.LE.1 ) THEN
          CALL GTFSEG(IMOD,LADDER(ILYR),CONT)
          IADD=ICONT(2)
          CALL FCODER(IADD,H,UNIT,QUAD,SECTOR,WIRE,UB,1)
          CALL GTFALH(HALF,UNIT,QUAD,SECTOR,0,XC,YC,ZC)
          X2SUM=X2SUM+XC**2.
          Y2SUM=Y2SUM+YC**2.
          NLYR=NLYR+1
          TRKRAD=0.0
        ELSEIF (NLYR.GT.0) THEN
          X02=X2SUM/FLOAT(NLYR)
          Y02=Y2SUM/FLOAT(NLYR)
          TRKRAD=SQRT(X02**2. + Y02**2.)
        ENDIF
        CALL PF3DSGDR(IMOD,LADDER(ILYR),TRKRAD) ! Draw each segment w hits.
        CALL FSGXYZ(IMOD,LADDER(ILYR),X,Y,Z,OK)
        IF (.NOT. OK) GOTO 10
        CALL FPLANE(3,X,Y,Z,A(ILYR),B(ILYR),C(ILYR),D(ILYR),IER)
        IF (IER.NE.0) GOTO 10
        IF (IVIEW.EQ.2) GOTO 10
        CALL PFPLAN(HALF,A(ILYR),B(ILYR),C(ILYR),D(ILYR))       ! Draw plane.
   10 CONTINUE
C
C  Draw intersection lines between each pair of planes.
C
      CALL PFILIN(HALF,A(0),B(0),C(0),D(0),A(1),B(1),C(1),D(1),
     &            X1,Y1,Z1,X2,Y2,Z2)
      CALL PFILIN(HALF,A(2),B(2),C(2),D(2),A(1),B(1),C(1),D(1),
     &            X1,Y1,Z1,X2,Y2,Z2)
      CALL PFILIN(HALF,A(2),B(2),C(2),D(2),A(0),B(0),C(0),D(0),
     &            X1,Y1,Z1,X2,Y2,Z2)
C
C  Fit segments to a track and display results.
C
      CALL FTFDCT(HALF,LADDER,QTRAK,IQTRAK,QHSEC,IQHSEC,
     &  CHINORM,.FALSE.)
      IF (IQTRAK(2).LE.0) GOTO 200
      IB0=IBITS(IQTRAK(3),0,8)
      IB1=IBITS(IQTRAK(3),8,8)
      IB2=IBITS(IQTRAK(3),16,8)
      IB3=IBITS(IQTRAK(3),24,8)
      WRITE(FTEXT,101)
      CALL PF3MES(FTEXT)
      WRITE (FTEXT,102) ITRACK,IB3,IB2,IB1,IB0,IQTRAK(2),
     X      (QTRAK(IWORD),IWORD=4,6),QTRAK(22),QTRAK(7),
     X      QTRAK(8),QTRAK(19)
      CALL PF3MES(FTEXT)
  101 FORMAT('  FDCT    b3 b2 b1 b0   NHIT    x0
     X    y0       phi     theta    dx/dz    dy/dz  chisq ')
  102 FORMAT(1X,I4,4X,4I3,I6,6F9.3,E9.2)
C
C  Draw track up to chamber and sections of the track through sectors.
C
      DIR=ABS(Z0(HALF+1))/Z0(HALF+1)
      CALL PXCOLR(TITCLR)
      CALL J3MOVE(0.,0.,0.)
      CALL J3DRAW(10.,0.,0.)
      CALL J3STRG('>')
      CALL J3MOVE(0.,0.,0.)
      CALL J3DRAW(0.,10.,0.)
      CALL J3STRG('^')
      CALL J3MOVE(QTRAK(4),QTRAK(5),Z0(HALF+1))
      CALL JR3MOV(DXDZ*(-5.*DIR),DYDZ*(-5.*DIR),(-5.*DIR))
      X0=QTRAK(4)-DXDZ*(Z0(HALF+1)-5.*DIR)
      Y0=QTRAK(5)-DYDZ*(Z0(HALF+1)-5.*DIR)
      CALL J3DRAW(X0,Y0,0.)
      CALL J3DRAW(X0+10.,Y0,0.)
      CALL J3MOVE(X0,Y0,0.)
      CALL J3DRAW(X0,Y0+10.,0.)
      CALL GTFALH(HALF,0,0,0,7,XC,YC,ZWB(2))
      CALL GTFALH(HALF,0,0,0,0,XC,YC,ZWE(2))
      CALL GTFALH(HALF,0,4,0,0,XC,YC,ZWB(2))
      CALL GTFALH(HALF,0,4,0,7,XC,YC,ZWE(2))
      CALL GTFALH(HALF,1,0,0,0,XC,YC,ZWB(2))
      CALL GTFALH(HALF,1,0,0,15,XC,YC,ZWE(2))
      DO 30 ILYR=0,2
        XTRK=QTRAK(4)+DXDZ*(ZWB(ILYR)-Z0(HALF+1))
        YTRK=QTRAK(5)+DYDZ*(ZWB(ILYR)-Z0(HALF+1))
        CALL J3MOVE(XTRK,YTRK,ZWB(ILYR))
        CALL JR3DRA(DXDZ*(ZWE(ILYR)-ZWB(ILYR)),
     &          DYDZ*(ZWE(ILYR)-ZWB(ILYR)),(ZWE(ILYR)-ZWB(ILYR)))
   30 CONTINUE
C
C  If a new track can be formed by the chosen segments, save it in Zebra.
C
      IF (FCHEKL(HALF,LADDER)) GO TO 200
C
      IF (CHINORM.LT.CHIMAX) THEN
        CALL LDFDCT(QTRAK,QHSEC,LADDER)  ! Store track
        CALL FUSDSG(HALF,LADDER)         ! Mark hits on track
      END IF
C
  200 CONTINUE
C
C  Done.
C
      GOTO 999
C
C  Error condition messages and handling.
C
  900 CONTINUE
      CALL OUTMSG(' No hits on this track!!?????')
      GOTO 999
C
C----------------------------------------------------------------------
  999 RETURN
      END
