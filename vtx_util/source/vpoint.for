      SUBROUTINE VPOINT(LAYER,SECTOR)
C------------------------------------------------------------------------  
C
C  Find coordinates r,phi and x,y,z of hits in current cell (LAYER,SECTOR),
C  in VTX.  Store them in Zebra bank 'POIN'.
C  (used in the 'road' method of finding track segments)
C
C  D.Zieminska  Jan. 1988
C               Oct. 1988: use VTX_STPFILE.DAT  
C-   Updated  21-MAY-1992   Peter M. Grudberg Double bank size to handle mirror
C-                                            hits and avoid putting in too many
C-                                            hits.
C-   Updated  30-AUG-1992   Finds point in the frame where the beam position
C-                                            is at (0,0)
C-   updated  31-aug-1992   L. Chen use the wire dX/dZ and dY/dZ in VALS.ZEB
C-   Updated  25-SEP-1992   Peter M. Grudberg Branch cut for layer 0 fixed
C-                            (wrong sector was checked)
C-   Updated  19-OCT-1992   Peter M. Grudberg  Avoid duplicate space points;
C-                                      clean up
C-   Updated  11-FEB-1993   Ed Oltman   Use z-info from road definition (if it
C-                          exists) and utilize dx/dz,d2x/dz2 etc. of wires
C-   Updated  16-JUL-1993   Liang-ping Chen use VERTXMC, VERTYMC for MC   
C-   Updated   6-FEB-1995   Ed Oltman   USE VXY_BEAM1 INSTEAD OF VTRAKS.RCP.
C-                          ..GET RID OF UNUSED VARIABLES..
C-   Updated  16-FEB-1995   Liang-ping Chen  protect ZVTX=9999 with VXY_BEAM1
C------------------------------------------------------------------------
      IMPLICIT NONE           
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
      INCLUDE 'D0$INC:VTXLNK.INC/LIST'                             
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'                             
      INCLUDE 'D0$LINKS:IZUSER.LINK/LIST'                             
      INTEGER LAYER,SECTOR,LUSER 
      INTEGER NWIRES,NBSENS,NLAYER,NEL,NWORDS
      PARAMETER (NWIRES=7)      ! maximum wire# (counting from 0)
      PARAMETER (NBSENS=8)      ! number of sense wires 
      PARAMETER (NLAYER=2)      ! maximum layer# (counting from 0)
      REAL CONT(18),STAT
      INTEGER MMBOOK(5)
      INTEGER NHITS(0:NWIRES),IPTR(0:NWIRES),MXHSEC,MXHWIR
      PARAMETER (MXHSEC=200)
      PARAMETER (MXHWIR=50)
      INTEGER I1,LR
      INTEGER  IPOINT,FIRST,LAST,STEP,ICALL,ISTAT,NTHIT(NBSENS) 
      INTEGER  WIRE, IHIT, ILABEL 
      INTEGER  IXPOIN,LPOIN,LOC,LVALS,IPAL,IER
      INTEGER LVRFT, GZVRFT, GZVALS
      REAL     DRIFT,XHIT,YHIT,PHI,FLABEL
      REAL LAST_DRIFT, STAGGER
      REAL PI2
      INTEGER STATUS
      LOGICAL SLOPE_CORRECT
      REAL  PHIR,DZDR,ZVTX,ZPRED
      PARAMETER   ( PI2=6.283185 )
      EQUIVALENCE ( ILABEL, FLABEL )
      EQUIVALENCE ( ISTAT, STAT )
C
      INTEGER IQHIT(18*MXHSEC)
      EQUIVALENCE (IQHIT,QHIT)
      REAL QHIT(18*MXHSEC),VERTX,VERTY, ZVTX_LAST,DX,DY
      INTEGER RUN,EVENT,RUNNO,EVONUM,EVENT_LAST,RUN_LAST
C                                     
      DATA ICALL/0/
C------------------------------------------------------------------------
C
      CALL VZERO( NTHIT, NBSENS )
      IF (ICALL.EQ.0) THEN
        CALL EZPICK( 'VTRAKS_RCP' )
        CALL EZGET('SLOPE_CORRECT',SLOPE_CORRECT,IER)
        CALL EZRSET
        CALL MZFORM('POIN','8I/3F 1B 4F',IXPOIN)
        CALL UCTOH('POIN',MMBOOK(1),4,4)
        MMBOOK(2)=0 
        MMBOOK(3)=0 
        MMBOOK(4)=8+2*8*MXHWIR*NBSENS ! The 2 is for the mirror hits
        MMBOOK(5)=IXPOIN 
        ICALL=1
        EVENT_LAST = -1
        RUN_LAST = -1
        ZVTX_LAST = -999.
      END IF
      LUSER=LQ(LHEAD-IZUSER) 
      CALL GTVSEC(LAYER,SECTOR,'WIR',0,NEL,NWORDS,CONT)
      CALL UCOPY(CONT,NHITS,NEL)
      CALL UCOPY(CONT(NEL+1),IPTR,NEL)
      CALL GTVSEC(LAYER,SECTOR,'ALL',IPTR(0),NEL,NWORDS,QHIT)
      CALL MZLIFT(0,LPOIN,LUSER,-1,MMBOOK,0)  
      LOC=LPOIN+8
      LVALS = GZVALS(LAYER,SECTOR)
      LVRFT = GZVRFT()
C
      EVENT = EVONUM()
      RUN   = RUNNO()
      IF (EVENT .NE. EVENT_LAST .OR. RUN .NE. RUN_LAST) THEN
        RUN_LAST = RUN
        EVENT_LAST = EVENT
        CALL VRDGET(ZVTX,PHIR,DZDR)
        ZVTX_LAST = ZVTX
        IF (ZVTX.GE.9999) ZVTX=0.
        CALL VXY_BEAM1(ZVTX,VERTX,DX,VERTY,DY,STATUS)
      ELSE
        CALL VRDGET(ZVTX,PHIR,DZDR)
        IF (ZVTX .NE. ZVTX_LAST) THEN
          ZVTX_LAST = ZVTX
          IF (ZVTX.GE.9999) ZVTX=0.
          CALL VXY_BEAM1(ZVTX,VERTX,DX,VERTY,DY,STATUS)
        ENDIF
      ENDIF

      DO 200 WIRE=0,NWIRES
        IF(NHITS(WIRE).EQ.0) GO TO 200
        IPAL = LVALS + 6 + IC(LVALS+6)*WIRE
        STAGGER = C(LVRFT+31+WIRE)
        IF ( MOD(SECTOR,2) .EQ. 1 ) STAGGER = - STAGGER
        IPOINT=0
        LAST_DRIFT = -9999.
        DO 300 LR=1,0,-1 
          IF (LR.EQ.0) THEN
            FIRST=1
            LAST=MIN(NHITS(WIRE),MXHWIR)
            STEP=1
          ELSE
            FIRST=MIN(NHITS(WIRE),MXHWIR)
            LAST=1
            STEP=-1
          END IF
          DO 400 IHIT=FIRST,LAST,STEP        
            I1=IPTR(WIRE)-2*NEL-4+NWORDS*(IHIT-1)
            STAT=QHIT(I1+10)
            IF (BTEST(ISTAT,2)) GO TO 400
            DRIFT=QHIT(I1+2+LR) - STAGGER
            IF ( DRIFT .EQ. LAST_DRIFT ) GO TO 400
            LAST_DRIFT = DRIFT
            XHIT = C(IPAL+1)+DRIFT*C(LVALS+3)- VERTX         ! BEAM FRAME 
            YHIT = C(IPAL+2)+DRIFT*C(LVALS+4)- VERTY
C
C ****  If requested, make correction for slope of wire in D0 frame (dx,dz).
C ****  Demand that the hit has hits on both ends.
C
C
C ****  BUT FIRST, IF ROAD INFO EXISTS, USE THAT
C
            IF (ABS(ZVTX) .LT. 999. ) THEN
              ZPRED = ZVTX + SQRT(XHIT**2 + YHIT**2)*DZDR
              XHIT = XHIT + C(IPAL+4)*ZPRED + C(IPAL+6)*ZPRED**2
              YHIT = YHIT + C(IPAL+5)*ZPRED + C(IPAL+7)*ZPRED**2
            ELSE
              STATUS = IBITS(IQHIT(I1+10),0,2)
              IF ( STATUS .EQ. 3 .AND. SLOPE_CORRECT ) THEN
                XHIT = XHIT + C(IPAL+4)*QHIT(I1+4)
                YHIT = YHIT + C(IPAL+5)*QHIT(I1+4)
              ENDIF
            ENDIF
C
            IPOINT=IPOINT+1
            Q(LOC+1)=SQRT(XHIT**2+YHIT**2) ! r 
            PHI=ATAN2(YHIT,XHIT)      
            IF (LAYER.EQ.0.AND.SECTOR.EQ.8.AND.PHI.LT.0.) PHI=PHI+PI2
            IF (LAYER.EQ.1.AND.SECTOR.EQ.16.AND.PHI.LT.0.) PHI=PHI+PI2
            IF (LAYER.EQ.2.AND.SECTOR.EQ.16.AND.PHI.LT.0.) PHI=PHI+PI2
            Q(LOC+2)=PHI 
            Q(LOC+3)=1./(QHIT(I1+5))**2    ! wt=1/dy**2 
            Q(LOC+5)=XHIT
            Q(LOC+6)=YHIT
            Q(LOC+7)=QHIT(I1+4)            ! z 
            Q(LOC+8)=1./(QHIT(I1+6))**2    ! wtz=1/dz**2
            I1=I1+2*NEL+4
            ILABEL=I1*16+WIRE*2+LR
            IQ(LOC+4)=ILABEL
            LOC=LOC+8
  400     CONTINUE
  300   CONTINUE
        IQ(LPOIN+1+WIRE)=IPOINT
  200 CONTINUE
 1000 RETURN                  
      END
