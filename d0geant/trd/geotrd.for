      LOGICAL FUNCTION GEOTRD ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C
C     Geometry definition of the 'TRD ' detector volume:
C       Position the elementary volumes defined in TRDCON
C
C           If TRD geometry banks are present, then:
C           Take positioning from them
C               (FOR THE TIME BEING ONLY TRANSLATIONS IMPLEMENTED)
C           If TRD geometry banks are absent,everything is given
C           by DATA statements as in previous versions.
C
C     Define as a 'TUBE' positioned in NMCEN
C     Volume identifiers start with 'T'
C     Material, Media US 70-79
C       and Rotations use 7000-7999
C
C     The structure of TRD geometory is following:
C     TRD_ : TRD mother volume
C       --> TRD1 : first module (inside)
C             --> TWI1 : inside wall (carbon fibers)
C             --> TRC1 : radiator cell (polypropylene + nitrogen)
C             --> TGC1 : gap cell (mylar + nitrogen)
C             --> TXC1 : X-ray detector cell (xenon + CH4 + wires)
C                 --> CEL1 : Drift cell       -OBSOLETE(23/12/87)
C             --> TWA1 : inner coating (G10)
C             --> TWO1 : outside wall (honeycomb)
C             --> TWB1 : external coating (G10)
C             --> TUG1 : upstream end cap G10
C             --> TUA1 : upstream end cap Aluminum in radiator
C             --> TUF1 : upstream end cap foam
C             --> TUK1 : upstream end cap Kevlar
C             --> TUH1 : upstream end cap honeycomb
C             --> TUI1 : upstream end cap inserts in honeycomb (aluminum
C             --> TUC1 : Upstream Vertex cables,TRD cables(inner layers)
C                        and shielding
C             --> TDG1 : downstream end cap G10
C             --> TDA1 : downstream end cap Aluminum in radiator
C             --> TDF1 : downstream end cap foam
C             --> TDK1 : downstream end cap Kevlar
C             --> TDH1 : downstream end cap honeycomb
C             --> TDI1 : downstream end cap inserts in honeycomb (alumin
C             --> TDC1 : Downstream Vertex cables,TRD cables(inner layers)
C                        and shielding
C       --> TRD2 : second module
C             --> TWI2,TRC2,TWM2,TXC2,TWA2,TWO2,TWB2,
C                 TUG2,TUA2,TUF2,TUK2,TUH2,TUI2,TUC2
C                 TDG2,TDA2,TDF2,TDK2,TDH2,TDI2,TDC2
C       --> TRD3 : third module (most outside)
C             --> TWI3,TRC3,TWM3,TXC3,TWA3,TWO3,TWB3,
C                 TUG3,TUA3,TUF3,TUK3,TUH3,TUI3,TUC3
C                 TDG3,TDA3,TDF3,TDK3,TDH3,TDI3,TDC3
C
C-
C-   Inputs  :
C-   Outputs :
C-
C-   Created  19-JUN-1986   SK, KN, SLL
C-   Updated   6-NOV-1987   MANSOULIE
C-   Updated  23-DEC-1987   A. ZYLBERSTEJN  SUPPRESS THE CELL DIVISION
C-   Updated  11-MAR-1988   B. MANSOULIE    Change to ZEBSTP VER 0.2
C-   Updated  17-JUL-1989   Harrison B. Prosper
C-   Made into pbd interface function; added call to DETTRD.
C-   Updated  24-FEB-1992   Y. Ducros: add volumes at the end of each layer
C-                            to reproduce more exactly the material repres-
C-                            ented by cables and extra shielding added to
C-                            the front-end electronics
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C  Detector parameters, from which Rmin, Rmax and half z are calculated.
C
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:TRDVOL.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC'
      INCLUDE 'D0$LINKS:IZTACH.LINK'
C
      INTEGER IM,ISM,IVOLU,IS,LTACH,LTRAC
      REAL XTRD1(3)
      REAL XTRD2(3,3)
C          All shapes  of the volumes are 'TUBE'.
C                Volume names.
      CHARACTER*4 CHTRD1,CHTRD2(3),CHTRD3(21,3)
C
C   coordinates of the center of each detector module.
      DATA XTRD1/3*0.0/
      DATA XTRD2/9*0.0/
C                      Volume names.
      DATA CHTRD1/ 'TRD '/
      DATA CHTRD2/ 'TRD1', 'TRD2', 'TRD3' /
      DATA CHTRD3/ 'TWI1','TRC1','TWM1','TXC1','TWA1','TWO1','TWB1',
     +             'TUG1','TUA1','TUF1','TUK1','TUH1','TUI1','TUC1',
     +             'TDG1','TDA1','TDF1','TDK1','TDH1','TDI1','TDC1',
     +             'TWI2','TRC2','TWM2','TXC2','TWA2','TWO2','TWB2',
     +             'TUG2','TUA2','TUF2','TUK2','TUH2','TUI2','TUC2',
     +             'TDG2','TDA2','TDF2','TDK2','TDH2','TDI2','TDC2',
     +             'TWI3','TRC3','TWM3','TXC3','TWA3','TWO3','TWB3',
     +             'TUG3','TUA3','TUF3','TUK3','TUH3','TUI3','TUC3',
     +             'TDG3','TDA3','TDF3','TDK3','TDH3','TDI3','TDC3'/
C
C----------------------------------------------------------------------
      GEOTRD = .TRUE.
      IF ( DTRD .LT. 1 ) GOTO 999
C
C                     Define rotation.
C
      CALL GSROTM(7000,90.,0.,90.,90.,0.,0.)
      CALL GSROTM(7001,90.,0.,90.,90.,0.,0.)
C
      IF(LTGEO.EQ.0) THEN
        WRITE(LOUT,1001)
      ELSE
        LTACH=LC(LTGEO-IZTACH)
        CALL UCOPY(C(LTACH+3),XTRD1,3)
        DO 100 IM=1,3
          LTRAC=LC(LTACH-IM)
          CALL UCOPY(C(LTRAC+1),XTRD2(1,IM),3)
  100   CONTINUE
      ENDIF
C
C                       Define media.
C
      CALL GSMATE(70,'AIR$',14.61,7.3,1.20E-3,30550.,75000.,0,0)
C
C        X0 AND L_int  for carbon fiber are taken from Berkeley group
      CALL GSMATE(71,'CARBON_FIBER$',12.01,6.0,1.50,42.7,86.3,0,0)
C
C          Radiator = CH2 (382*18 microns) + N2 (382*150 microns)
      CALL GSMATE(72,'RADIATOR$',12.,6.,0.0965,451.6,816.5,0,0)
C
      CALL GSMATE(73,'MID_WALL$',14.,7.,0.036,1107.,2382.,0,0)
      CALL GSMATE(74,'XE+CH4$',131.3,54.,0.00501,1711.,33200.,0,0)
      CALL GSMATE(75,'G10$',16.,8.,1.70,19.4,53.1,0,0)
      CALL GSMATE(76,'HONEYCOMB$',12.,6.,0.045,800.,1720.,0,0)
C
      CALL GSMATE(77,'ALUMINUM$',26.98,13.,2.70,8.9,39.4,0,0)
      CALL GSMATE(78,'FOAM$',12.,6.,0.071,570.,1200.,0,0)
      CALL GSMATE(79,'KEVLAR$',12.,6.,1.4,30.,40.,0,0)
C
C                  Define Tracking media.
C
      CALL GSTMED(70,'AIR$',     70,0,0,0.,90.,1.,1.,.05,0.2,0,0)
      CALL GSTMED(71,'IN_WALL$', 71,0,0,0.,90.,1.,1.,.05,0.2,0,0)
      CALL GSTMED(72,'RADIATOR$',72,0,0,0.,90.,1.,1.,.05,0.2,0,0)
      CALL GSTMED(73,'MID_WALL$',73,0,0,0.,90.,1.,1.,.05,0.2,0,0)
      CALL GSTMED(74,'X-RAY$',   74,1,0,0.,90.,1.,1.,.05,0.2,0,0)
      CALL GSTMED(75,'G10$',     75,0,0,0.,90.,1.,1.,.03,.04,0,0)
      CALL GSTMED(76,'OUT_WALL$',76,0,0,0.,90.,1.,1.,.05,0.2,0,0)
C
C                    End-cap volumes.
      CALL GSTMED(77,'ALU$',     77,0,0,0.,90.,1.,1.,.03,.04,0,0)
      CALL GSTMED(78,'FOAM$',    78,0,0,0.,90.,1.,1.,.05,0.2,0,0)
      CALL GSTMED(79,'KEVLAR$',  79,0,0,0.,90.,1.,1.,.05,0.2,0,0)
C                    Volume definition.
C
C                    (first level)
      CALL GSVOLU(CHTRD1,'TUBE',70,TRDVL1(1),3,IVOLU)
C                     (second level)
      DO 200 IM=1,3
        CALL GSVOLU(CHTRD2(IM),'TUBE',70,TRDVL2(1,IM),3,IVOLU)
  200 CONTINUE
C           (third level- layer in each detector)
      DO 210 IM=1,3
        CALL GSVOLU(CHTRD3(1,IM),'TUBE',71,TRDVL3(1,1,IM),3,IVOLU)
        CALL GSVOLU(CHTRD3(2,IM),'TUBE',72,TRDVL3(1,2,IM),3,IVOLU)
        CALL GSVOLU(CHTRD3(3,IM),'TUBE',73,TRDVL3(1,3,IM),3,IVOLU)
        CALL GSVOLU(CHTRD3(4,IM),'TUBE',74,TRDVL3(1,4,IM),3,IVOLU)
        CALL GSVOLU(CHTRD3(5,IM),'TUBE',75,TRDVL3(1,5,IM),3,IVOLU)
        CALL GSVOLU(CHTRD3(6,IM),'TUBE',76,TRDVL3(1,6,IM),3,IVOLU)
        CALL GSVOLU(CHTRD3(7,IM),'TUBE',75,TRDVL3(1,7,IM),3,IVOLU)
        CALL GSVOLU(CHTRD3(8,IM),'TUBE',75,TRDVL3(1,8,IM),3,IVOLU)
        CALL GSVOLU(CHTRD3(9,IM),'TUBE',77,TRDVL3(1,9,IM),3,IVOLU)
        CALL GSVOLU(CHTRD3(10,IM),'TUBE',78,TRDVL3(1,10,IM),3,IVOLU)
        CALL GSVOLU(CHTRD3(11,IM),'TUBE',79,TRDVL3(1,11,IM),3,IVOLU)
        CALL GSVOLU(CHTRD3(12,IM),'TUBE',76,TRDVL3(1,12,IM),3,IVOLU)
        CALL GSVOLU(CHTRD3(13,IM),'TUBE',77,TRDVL3(1,13,IM),3,IVOLU)
        CALL GSVOLU(CHTRD3(14,IM),'TUBE',77,TRDVL3(1,14,IM),3,IVOLU)
        CALL GSVOLU(CHTRD3(15,IM),'TUBE',75,TRDVL3(1,15,IM),3,IVOLU)
        CALL GSVOLU(CHTRD3(16,IM),'TUBE',77,TRDVL3(1,16,IM),3,IVOLU)
        CALL GSVOLU(CHTRD3(17,IM),'TUBE',78,TRDVL3(1,17,IM),3,IVOLU)
        CALL GSVOLU(CHTRD3(18,IM),'TUBE',79,TRDVL3(1,18,IM),3,IVOLU)
        CALL GSVOLU(CHTRD3(19,IM),'TUBE',76,TRDVL3(1,19,IM),3,IVOLU)
        CALL GSVOLU(CHTRD3(20,IM),'TUBE',77,TRDVL3(1,20,IM),3,IVOLU)
        CALL GSVOLU(CHTRD3(21,IM),'TUBE',77,TRDVL3(1,21,IM),3,IVOLU)
  210 CONTINUE
C             (forth level -drift cell)REMOVED(23/12/87)
C      CALL GSDVN('CEL1','TXC1',256,2)
C      CALL GSDVN('CEL2','TXC2',256,2)
C      CALL GSDVN('CEL3','TXC3',256,2)
C
C                 Positioning all volumes.
C
C               (first level into MCEN)
      CALL GSPOS(CHTRD1,1,'MCEN',XTRD1(1),XTRD1(2),XTRD1(3),
     +                 7001,'ONLY')
C              (second level- three modules)
      DO 300 IM=1,3
        CALL GSPOS(CHTRD2(IM),1,'TRD ',
     +           XTRD2(1,IM),XTRD2(2,IM),XTRD2(3,IM),7000,'ONLY')
  300 CONTINUE
C               (third level- seven layers)
      DO 310 IM=1,3
        DO 312 ISM=1,21
          CALL GSPOS(CHTRD3(ISM,IM),1,CHTRD2(IM),
     +          0.0, 0.0, ZCENT(ISM), 7000, 'ONLY')
  312   CONTINUE
  310 CONTINUE
C
      CALL GSATT('TXC1','SEEN',0)
      CALL GSATT('TXC2','SEEN',0)
      CALL GSATT('TXC3','SEEN',0)
C
C
C ****  Define detector sets and digitization
C
      IF ( DTRD .GE. 2 ) CALL DETTRD
C
C ****  Now order the volumes to speed things up
C
      CALL GSORD('TRD ',4)
      CALL GSORD('TRD1',3)
      CALL GSORD('TRD2',3)
      CALL GSORD('TRD3',3)
  999 RETURN
 1001 FORMAT(//,'  **** TRD GEOMETRY BANKS ARE ABSENT ****')
      END
