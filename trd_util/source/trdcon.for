      SUBROUTINE TRDCON(FILNAM,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C     Geometry definition of the 'TRD ' detector volume:
C      Define the elementary volumes (to be positioned in GEOTRD)
C
C-
C-   Inputs  :
C-   Outputs :
C-
C-   Created                        A. ZYLBERSTEJN
C-   Updated   7-DEC-1991   A. Zylberstejn
C-   Updated     FEB-1992   A. Pluquet: use GZTACH in order to get the
C-                                      valid stp file version
C-   Updated     FEB-1992   Y. Ducros:Define geometry for new volumes located in
C-                                    the flanges of each layer
C
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCONST.INC/LIST'
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INCLUDE 'D0$INC:GEOMTC.INC/LIST'
      INCLUDE 'D0$INC:GEOMTR.INC/LIST'
      INCLUDE 'D0$INC:TCNTRL.INC'
      INCLUDE 'D0$INC:TRDVOL.INC/LIST'
      INCLUDE 'D0$INC:TRDBGU.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INCLUDE 'D0$LINKS:IZTACH.LINK'
C
      CHARACTER*26 FILNAM
      INTEGER I,IER,IERR,IM,ISM,LTACH,LTRAC,NWS(3),ICASE,GZTACH
      REAL DA,DOFSPH(3),RRMIN(3),DRTRD(3)
      REAL   DRDRFT, DRMIN(21),DDEPTH(21),DZHALF(21,3)
      REAL ZMAX1,ZMAX2,ZACTD(3),ZCENTD(21)
C
      LOGICAL DOPRINT,TRD_DO_PRINT
      REAL WCAT(3),DZCAT(3),DRCAT(3),THETAC(3)
      INTEGER NSTR(3),TRUNIT
      DATA ICASE/0/
      DATA RRMIN/ 17.50, 28.05, 38.60/ !Most in side radius in each module.
      DATA WCAT /0.6, 0.8, 0.79939/    !Cathode strip width in each module
      DATA DRCAT /3*0.005/             !Half-thickness of cathode strips
      DATA DZCAT /3*84.8/              !Distance in Z from cathode read-out
                                       !  to mid-plane
      DATA NSTR /3*256/                !Number of strips in each module
      DATA DRTRD/3*10.35/              !Maximum depth in each module
      DATA ZMAX1,ZMAX2 /94.4, 94.4/    !Maximum distance along beam
C        Rmin of each sub-module in each detector.
      DATA DRMIN / 0., 0.1, 6.7, 6.9, 9.2, 9.25, 10.25,
     1             0., 4.65, 4.65, 6.85, 9.15, 10.25,0.,
     2             0., 4.65, 4.65, 6.85, 9.15, 10.25,0./
C        Depth of each sub-module in each detector.
      DATA DDEPTH /0.10, 6.60, 0.20, 2.30, 0.05, 1.00, 0.05,
     1            10.40, 2.20, 2.20, 2.30, 1.1, 0.1,10.4,
     2            10.40, 2.20, 2.20, 2.30, 1.1, 0.1,10.4/
C        Position in Z of center of sub-module.
      DATA ZCENTD /7*0.0, 92.95, 91.45, 88.45, 88.7, 87.7, 87.7,94.05,
     1                  -92.95,-91.45,-88.45,-88.7,-87.7,-87.7,-94.05/
C        Half-length along Z of sub-module.
      DATA DZHALF /7*83.2,0.75, 0.75, 2.25, 3.50, 4.5, 4.5, 0.145,
     1                    0.75, 0.75, 2.25, 3.50, 4.5, 4.5, 0.145,
     1                    7*83.2,0.75, 0.75, 2.25, 3.50, 4.5, 4.5, 0.
     1                    135,
     1                    0.75, 0.75, 2.25, 3.50, 4.5, 4.5, 0.135,
     1                    7*83.2,0.75, 0.75, 2.25, 3.50, 4.5, 4.5, 0.
     1                    125,
     1                    0.75, 0.75, 2.25, 3.50, 4.5, 4.5, 0.15/
C
C        Chamber parameters(not for GEANT tracking,for detector simul)
      DATA DRDRFT / 1.5/ , NWS /256,256,512/ , ZACTD/3*83.3/
C     ---------------------------------------------------------------
      IF(IQ(LHEAD+1) .LE. 1000)THEN ! data
        LOUT=TRUNIT()
        DOPRINT=TRD_DO_PRINT()
      ELSE  ! MC
        DOPRINT=.TRUE.
      END IF
C  Calculate volume parameters.
C               loop over each TRD module.
C      IF(ICASE.EQ.0)GO TO 216
      DO 100 IM=1,3 !  loop over submodule.
        DO 110 ISM=1,21
          TRDVL3(1,ISM,IM)=RRMIN(IM)+DRMIN(ISM)
          TRDVL3(2,ISM,IM)=TRDVL3(1,ISM,IM)+DDEPTH(ISM)
          TRDVL3(3,ISM,IM)=DZHALF(ISM,IM)
          ZCENT(ISM)=ZCENTD(ISM)
  110   CONTINUE
C              Volume parameter for each module.
        TRDVL2(1,IM)=RRMIN(IM)
        TRDVL2(2,IM)=TRDVL2(1,IM)+DRTRD(IM)
        TRDVL2(3,IM)=ZMAX2
        IF(ZMAX2.GT.ZMAX1) ZMAX1=ZMAX2
  100 CONTINUE
C            Volume parameter for TRD,  (mother volume of TRD).
      TRDVL1(1)=TRDVL2(1,1)
      TRDVL1(2)=TRDVL2(2,3)
      TRDVL1(3)=ZMAX1
C
C
C     ---------------------------------------------------------------
      DO 200 IM=1,3
C  EXIT WINDOW RADIUS
        RADEXT(IM)=RRMIN(IM)+DRMIN(5)
C  GRID RADIUS
        RADGRI(IM)=RRMIN(IM)+DRMIN(4)+DRDRFT
C  RADIAL DISTANCE BETWEEN ANODE AND GRID
        DRGRAN(IM)=.5*(RADEXT(IM)-RADGRI(IM))
C  RADIAL POSITION OF SENSE WIRES
        RADAN(IM)=.5*(RADEXT(IM)+RADGRI(IM))
C  NUMBER OF WIRES
        NWIRE(IM)=NWS(IM)
C  DISTANCE BETWEEN 2 SENSE WIRES
        DISTAN(IM)=TWOPI*RADAN(IM)/NWIRE(IM)
C  DELTA (PHI) ,IN RADIANS,BETWEEN 2 SENSE WIRES
        DPHIAN(IM)=TWOPI/FLOAT(NWIRE(IM))
C  ANGULAR OFFSET IN PHI
        OFSDPH(IM)= 0.5*DPHIAN(IM)
C  NUMBER OF CATHODE STRIPSNUMBER OF CATHODE STRIPS
        NSTRIP(IM) = NSTR(IM)
C  WIDTH OF STRIPS
        WIDCAT(IM) = WCAT(IM)
C  DELTA PHI IN RADIANS BETWEEN 2 STRIPS
        DPHICA(IM) = TWOPI/FLOAT(NSTRIP(IM))
C  DISTANCE BETWEEN 2 STRIPS (ALONG PHI)
        DISTCA(IM) = TWOPI*RADEXT(IM)/FLOAT(NSTRIP(IM))
C  ANGLE OF CATHODE STRIPS W.R.T. BEAM
        THETAC(IM) = ACOS(FLOAT(NSTRIP(IM))*WIDCAT(IM)/
     1    (TWOPI*(RADEXT(IM)+DRCAT(IM))))
C  STRIPS OF CHAMBER 2 TURN OPPOSITE WAY
        THETAC(2) = -THETAC(2)
C  DPHI/DZ FOR CATHODE STRIPS
        DPHIDZ(IM) = TAN(THETAC(IM))/RADEXT(IM)
C  ANGULAR OFFSET IN PHI OF 1ST CATHODE STRIP
C        dzcat(im)=dzcat(im)+25.
        OFSCAT(IM) = -DZCAT(IM)*DPHIDZ(IM)
        IF (IM.GE.2) OFSCAT(IM) = OFSCAT(IM) - 0.5*DPHICA(IM)
C  WINDOW RADIUS(INSIDE THE T.E.C.)
        RADWIN(IM)=RRMIN(IM)+DRMIN(4)
C  RADIUS OF THE FIRST FOIL
        RADIN(IM)=RRMIN(IM)+DRMIN(2)
C  ACTIVE Z-LENGTH
        ZACT(IM)=ZACTD(IM)
C  RADIAL DISTANCE BETWEEN WINDOW AND GRID
        DRWING(IM)=-RADWIN(IM) +RADGRI(IM)
C  RADIAL DISTANCE BETWEEN WINDOW AND ANODE
        DRWINA(IM)=-RADWIN(IM)+RADAN(IM)
C
        IF (DOPRINT) THEN
          WRITE(LOUT,*)' STACK',IM,' RADIUS: FIRST FOIL =',RADIN(IM),
     +      ',WINDOW =',RADWIN(IM),',GRID =',RADGRI(IM),',ANODE =',
     +      RADAN(IM)
        ENDIF
  200 CONTINUE
  216 CONTINUE
C
C  ****************************
      CALL TRISTP( FILNAM,IERR)
C  ****************************
      IF(IERR.NE.0.OR.LTGEO.EQ.0) THEN
C        WRITE(LOUT,1001)
        CALL ERRMSG(' TRD GEOMETRY BANKS ARE ABSENT ','TRDCON',
     &    ' Keep hard wired values','W')
        GOTO 900
      ENDIF
      LTACH=GZTACH()    ! GZTACH gives automatically the correct stp file
      DO 300 IM=1,3
        LTRAC=LC(LTACH-IM)
        RADAN(IM)=C(LTRAC+18) !  RADIAL POSITION OF SENSE WIRES
        NWIRE(IM)=IC(LTRAC+11)!  NUMBER OF WIRES
        DISTAN(IM)=TWOPI*RADAN(IM)/NWIRE(IM)!DISTANCE BETWEEN 2 SENSE WIRES
        DPHIAN(IM)=TWOPI/FLOAT(NWIRE(IM)) !DELTA (PHI) BETWEEN 2 SENSE WIRES
        OFSDPH(IM)=C(LTRAC+20) !ANGULAR OFFSET IN PHI
        RADGRI(IM)=C(LTRAC+15) !GRID RADIUS
        RADWIN(IM)=C(LTRAC+13) !WINDOW RADIUS(INSIDE THE T.E.C.)
        RADEXT(IM)=C(LTRAC+21) !EXIT WINDOW RADIUS
        NSTRIP(IM) = IC(LTRAC+12)!NUMBER OF CATHODE STRIPS
        DISTCA(IM) = TWOPI*RADEXT(IM)/FLOAT(NSTRIP(IM))!DISTANCE BETWEEN
                                                       ! 2 STRIPS (ALONG PHI)
        DPHICA(IM) = TWOPI/FLOAT(NSTRIP(IM))!DELTA PHI IN RADIANS BETWEEN
                                            !   2 STRIPS
        DPHIDZ(IM) = C(LTRAC+24)  !DPHI/DZ FOR CATHODE STRIPS
        OFSCAT(IM) = C(LTRAC+25)  !ANGULAR OFFSET IN PHI OF 1ST CATHODE STRIP
        DRGRAN(IM)=RADAN(IM)-RADGRI(IM)!RADIAL DISTANCE BETWEEN ANODE AND GRID
        DRWING(IM)=-RADWIN(IM)+RADGRI(IM)!RADIAL DISTANCE BETWEEN WINDOW AND GRID
        DRWINA(IM)=-RADWIN(IM)+RADAN(IM) !RADIAL DISTANCE BETWEEN WINDOW AND
                                         !  ANODE
        ZACT(IM)=C(LTRAC+19)!ACTIVE Z-LENGTH
C        dzcat(im)=dzcat(im)-25.
C        OFSCAT(IM) = -DZCAT(IM)*DPHIDZ(IM)
C        IF (IM.GE.2) OFSCAT(IM) = OFSCAT(IM) - 0.5*DPHICA(IM)
C
C         Overwrite the volume parameters bound to the chambers
C
        TRDVL3(1,4,IM)=RADWIN(IM)
        TRDVL3(2,4,IM)=RADEXT(IM)
        TRDVL3(1,5,IM)=RADEXT(IM)
        TRDVL3(2,5,IM)=RADEXT(IM)+DDEPTH(5)
        TRDVL3(1,6,IM)=TRDVL3(2,5,IM)
        TRDVL3(2,6,IM)=TRDVL3(1,6,IM)+DDEPTH(6)
        TRDVL3(1,7,IM)=TRDVL3(2,6,IM)
        TRDVL3(2,7,IM)=TRDVL3(1,7,IM)+DDEPTH(7)
C
  300 CONTINUE
C
  900 CONTINUE
      ZMIN=ZACT(3)
      IF(DOPRINT)THEN
        WRITE(LOUT,*)' TRD geometrical constants in trdcon'
        WRITE(LOUT,*)'  Ofsdph',OFSDPH(1),OFSDPH(2),OFSDPH(3)
      ENDIF
      IF(IQ(LHEAD+1).GT.1000 .OR. IQ(LHEAD+1).LE.0)GO TO 999
      CALL EZPICK('TRD_RCP')
      CALL EZGET('PHI_OFFSET',DOFSPH(1),IER)
      IF(DOPRINT.AND.IER.NE.0)THEN
        WRITE(LOUT,*)' TRD chambers offsets not found, keep old values'
      ELSE
        OFSDPH(1)=OFSDPH(1)-DOFSPH(1)*DEGRAD
        OFSDPH(2)=OFSDPH(2)-DOFSPH(2)*DEGRAD
        OFSDPH(3)=OFSDPH(3)-DOFSPH(3)*DEGRAD
      END IF
      IF(COSMIC1)THEN ! for COSMIC 1 runs impose: Phi off-set=0
        OFSDPH(1)=0.
        OFSDPH(2)=0.
        OFSDPH(3)=0.
      END IF
      IF(DOPRINT)THEN
        WRITE(LOUT,*)' TRD geometrical constants in trdcon'
        WRITE(LOUT,*)'  Ofsdph',OFSDPH(1),OFSDPH(2),OFSDPH(3)
        WRITE(LOUT,*) ' Dofsph',DOFSPH(1),DOFSPH(2),DOFSPH(3)
        DA=TWOPI/256.
C      WRITE(LOUT,*)' Phi offset in cell phi unit'
C      WRITE(LOUT,'(3g10.4)')OFSDPH(1)/DA,
C     &  OFSDPH(2)/DA,OFSDPH(3)/DA
C      WRITE(LOUT,*)'  dzcat',DZCAT
C      WRITE(LOUT,*)'  ofscat',OFSCAT
      END IF
      CALL EZRSET
  999 RETURN
 1001 FORMAT(//,'  **** TRD GEOMETRY BANKS ARE ABSENT ****')
      END
