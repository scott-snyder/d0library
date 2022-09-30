      FUNCTION GEOCDC()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :   CENTRAL DRIFT CHAMBER GEOMETRY
C
C      Geometry definition of the 'DC  ' detector volume
C      Define as a 'TUBE' positioned in NMCEN
C      Volume identifiers start with 'D'
C      Material, Media USE 80-89
C         and Rotations use 8000-8999
C
C      Structure of the central drift chamber is:
C         MVOL : mother volume of D0.
C           --> CDC   : central drift detector (mother volume)
C                 --> DCWI : innerside wall of container
C                 --> DRFT : drift cell module
C                       --> DRF1 : first layer of drift cells
C                             --> DFS1 : 32 drift cells
C                       --> DRF2 : second layer of drift cells
C                             --> DFS2 : 32 drift cells
C                       --> DRF3 : third layer of drift cells
C                             --> DFS3 : 32 drift cells
C                       --> DRF4 : forth layer of drift sells
C                             --> DFS4 : 32 DRIFT CELLS
C                       --> DCDE(1): end plate (z>0)
C                       --> DCDE(2): end plate (z<0)
C                 --> DCWO : outerside wall of container
C                 --> DCCE(1): end plate of container
C                 --> DCCE(2): end plate of container
C           --> ECAN: extension parts of CD can
C           --> ECBL: VTX+TRD+CDC cables under the extension parts of CD can
C           --> ECDC: electronics + cables in front of CDC end plates
C-
C-   Returned value  : TRUE
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  DD-MMM-19YY
C-   Updated   9-JUN-1989   Harrison B. Prosper
C-   Made into program-builder interface function
C-   Updated  31-MAR-1992   Qizhong Li-Demarteau  added volume ECAN, ECBL
C-                                                and ECDC 
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:CENMV.INC/LIST'
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
C
      INCLUDE 'D0$LINKS:IZDWAL.LINK/LIST'
      INCLUDE 'D0$LINKS:IZDMAT.LINK/LIST'
      INCLUDE 'D0$LINKS:IZDRFT.LINK/LIST'
C
      LOGICAL GEOCDC
C
      INTEGER LDMAT, LDWAL, LDRFT
      INTEGER IVOLU
      INTEGER I, J, JDRF, IMED, IWED, NUM
      REAL ATW, ATN, DEN, RDL, ABL
      INTEGER KVLM, KVPAR
      REAL XCENT, YCENT, ZCENT, ANG, THIC, RMIN, R, Z
C
C **** Names of the cells in each layer
C
      CHARACTER*4 NAM,NAMOT
      CHARACTER*4 DRFNAM(4), DFSNAM(4),DIVSEC( 4, 3), DFTI(4)
      DATA DRFNAM / 'DRF1', 'DRF2', 'DRF3', 'DRF4' /
      DATA DFSNAM / 'DFS1', 'DFS2', 'DFS3', 'DFS4' /
      DATA DIVSEC / 'DF11', 'DF21', 'DF31', 'DF41' ,
     &              'DF12', 'DF22', 'DF32', 'DF42' ,
     &              'DF13', 'DF23', 'DF33', 'DF43' /
      DATA DFTI   / 'DFT1', 'DFT2', 'DFT3', 'DFT4' /
C
C **** Half Thickness of the cells
C
      INTEGER NDIV
      REAL THCEL(3), TDIV, SEPTU
      DATA NDIV / 5 /
      DATA THCEL / .355, 1.500, .355 /
      REAL ZPDRF, ANGCEL, NLAY, NSEC(4), X(10)
      CHARACTER*4 NAMEC
C----------------------------------------------------------------------
C
C ****  Machine dependent GSVECT calls
C
C&IF ETA10
C&C  add  GSVECT for speedy execution on Cyber, ---removable
C&      CALL GSVECT('DRF1')
C&      CALL GSVECT('DRF2')
C&      CALL GSVECT('DRF3')
C&      CALL GSVECT('DRF4')
C&ENDIF
C----------------------------------------------------------------------
      GEOCDC = .TRUE.
      IF ( DCDC .LT. 1 ) GOTO 999
C
C
C  Define rotation matix,  8000 = Position of the CDC in the D0 frame
C                          8100 = Position of all the volumes in the CDC
C                                 frame
C                          8200 = Position of the cells compared to CDC
C
C  ********** caution : 8000 should depend on the alignment
      CALL GSROTM(8000,90.,0.,90.,90.,0.,0.)
      CALL GSROTM(8100,90.,0.,90.,90.,0.,0.)
      CALL GSROTM(8200,90.,270.,180.,0.,90.,0.)
C
C  **** Define media.
C
      LDMAT = LC ( LDGEH - IZDMAT )
      IF ( LDMAT .LE. 0 ) THEN
        WRITE ( LOUT,*) ' routine GEOCDC: bank DMAT doesn''t exist:',
     &               ' impossible to define the material in the CDC'
        CALL EXIT(1)
      ENDIF
      IMED = IC ( LDMAT + 1 )
      IWED = IC ( LDMAT + 2 )
      LDMAT = LDMAT + 2
      DO 400 I = 1, IMED
        NUM = IC ( LDMAT + 1 )
        ATW = C ( LDMAT + 7 )
        ATN = C ( LDMAT + 8 )
        DEN = C ( LDMAT + 9 )
        RDL = C ( LDMAT + 10)
        ABL = C ( LDMAT + 11)
        CALL UHTOC_i(IC(LDMAT+2),4,NAMEC,4)
        CALL GSMATE ( NUM, NAMEC, ATW, ATN,
     &                   DEN, RDL, ABL, 0, 0 )
        LDMAT = LDMAT + IWED
  400 CONTINUE
C
C ****  Store the parameters of the tracking medium in the data structure
C ****  JTMED
C
      CALL GSTMED(80,'CDC_MOTHER$',  80,0,0,0.,90.,1.,1.,.05,.2,0,0)
      CALL GSTMED(81,'CDC_INWALL$',  81,0,0,0.,90.,1.,1.,.05,.2,0,0)
      CALL GSTMED(82,'CDC_CELLWALL$',82,0,0,0.,90.,1.,1.,.05,.2,0,0)
      CALL GSTMED(83,'CDC_CELLEND$', 83,0,0,0.,90.,1.,1.,.05,.2,0,0)
      CALL GSTMED(84,'CDC_GAS$',     84,1,0,0.,90.,1.,1.,.05,.2,0,0)
      CALL GSTMED(85,'CDC_ALMWALL$', 85,0,0,0.,90.,1.,1.,.05,.2,0,0)
      CALL GSTMED(86,'CABLES$',      85,0,0,0.,90.,1.,1.,.05,.2,0,0)
      CALL GSTMED(87,'ELEC+CABLES$', 85,0,0,0.,90.,1.,1.,.05,.2,0,0)
C
C  Define volumes : Informations are taken from the bank DWAL
C
      LDWAL = LC ( LDGEH - IZDWAL )
      IF ( LDWAL .LE. 0 ) THEN
        WRITE ( LOUT,*) ' routine GEOCDC: bank DWAL doesn''t exist:',
     &               ' impossible to define the geometry of the CDC'
        CALL EXIT(1)
      ENDIF
      KVLM  = IC ( LDWAL + 1 )
      KVPAR = IC ( LDWAL + 2 )
      LDWAL = LDWAL + 2
      XCENT = C ( LDGEH + 3 )
      YCENT = C ( LDGEH + 4 )
      ZCENT = C ( LDGEH + 5 )
      DO 100 I = 1, KVLM
        CALL UHTOC ( iC(LDWAL+1),4,NAM,4)
        CALL UHTOC ( iC(LDWAL+7),4,NAMOT,4)
        X(1) = C ( LDGEH + IC ( LDWAL + 2 ) )
        X(2) = C ( LDGEH + IC ( LDWAL + 3 ) )
        X(3) = C ( LDGEH + IC ( LDWAL + 5 ) )
        J = IC ( LDWAL + 4 )
        IF ( J .NE. 0 ) X(3) = ( X(3) - C(LDGEH+J)) / 2
        CALL GSVOLU ( NAM, 'TUBE', IC(LDWAL+6), X, 3, IVOLU )
        IF ( IVOLU .LE. 0 ) THEN
          WRITE ( LOUT, 500) NAM
          GO TO 999
        ENDIF
C
C ****  For the volume CDC, the position depend on the alignment; the other
C ****  volumes refer to the CDC volume.
C
        IF ( NAM . EQ. 'CDC' ) THEN
C       CHANGER LE 8000 EN UN AUTRE MATRICE DE ROTATION DE CDC / MAIN FRAME
          CALL UHTOC(NMCEN,4,NAMEC,4)
          CALL GSPOS (NAM,1,NAMEC,XCENT,YCENT,ZCENT,8000,'ONLY')
        ELSE
          IF (NAMOT .NE. 'MCEN') THEN
C
C ****  Do we need 1 or 2 copies of the volume ?
C
            IF ( IC(LDWAL+4) .EQ. 0 ) THEN
              CALL GSPOS (NAM,1,NAMOT, 0., 0., 0.,8100,'ONLY')
            ELSE
              Z = C(LDGEH + IC(LDWAL+4)) + C(LDGEH + IC(LDWAL+5))
              Z = Z / 2.
              CALL GSPOS (NAM,1,NAMOT, 0., 0., Z,8100,'ONLY')
              CALL GSPOS (NAM,2,NAMOT, 0., 0., -Z,8100,'ONLY')
            ENDIF
          ELSE
            IF ( IC(LDWAL+4) .EQ. 0 ) THEN
              CALL GSPOS (NAM,1,NAMOT, 0., 0., 0.,8000,'ONLY')
            ELSE
              Z = C(LDGEH + IC(LDWAL+4)) + C(LDGEH + IC(LDWAL+5))
              Z = Z / 2.
              CALL GSPOS (NAM,1,NAMOT, 0., 0., Z,8000,'ONLY')
              CALL GSPOS (NAM,2,NAMOT, 0., 0., -Z,8000,'ONLY')
            ENDIF
          ENDIF
        ENDIF
        LDWAL = LDWAL + KVPAR
  100 CONTINUE
C
C ****  Define the volumes contained in DRFT
C
      LDRFT = LC ( LDGEH - IZDRFT )
      IF ( LDRFT .LE. 0 ) THEN
        WRITE ( LOUT, * ) '****** GEOCDC : bank DRFT not defined'
        GO TO 999
      ENDIF
      NLAY = IC ( LDRFT + 1 )
      ZPDRF = C ( LDGEH + IC ( LDRFT + 7 ) )
      ANG   = C ( LDRFT + 9)
      ANGCEL =  TAN ( ANG * ACOS(-1.) / 180. )
      THIC   = C ( LDRFT + 8 )
      SEPTU  = C ( LDRFT + 10)
      JDRF = LDRFT + 10
      DO 200 I = 1, NLAY
        NSEC ( I )  = IC ( LDRFT + I + 1 )
        X( 1) = C(JDRF+2) - ANG
        X( 2) = NSEC(I) * 2. * ANG
        X( 3) = NSEC(I)
        X( 4) = 2.
        X( 5) = - ZPDRF
        X( 6) = C(JDRF+1) - THIC
        X( 7) = C(JDRF+1) + THIC
        X( 8) =   ZPDRF
        X( 9) = X(6)
        X(10) = X(7)
C
C ****  Creation and position of the volumes DRFi in each layer
C
        CALL GSVOLU( DRFNAM(I),'PGON',82,X,10,IVOLU)
        IF ( IVOLU .LE. 0 ) THEN
          WRITE ( LOUT, 500) DRFNAM(I)
          WRITE (6,*) X
          GO TO 999
        ENDIF
        CALL GSPOS ( DRFNAM(I),1,'DRFT', 0.,0.,0., 8100,'ONLY')
C
C ****  Devide into 32 cells.
C
        CALL GSDVN(DFSNAM(I),DRFNAM(I),32,2)
C
C ****  Creation  of the "cells" volumes - Let a septum of .01 cm between
C ****  the cells
C ****  subdivisions only, for the sense wires and the hits
C
        RMIN = C(JDRF+1) - THIC
        R = RMIN
        DO 300 J = 1, 3
          X(1) = R * ANGCEL - SEPTU
          X(2) = X(1) + THCEL ( J ) * 2. * ANGCEL
          X(3) = ZPDRF
          X(4) = THCEL ( J )
          CALL GSVOLU(DIVSEC(I,J),'TRD1',84,X,4,IVOLU)
          IF ( IVOLU .LE. 0 ) THEN
            WRITE ( LOUT, 500) DIVSEC(I,J)
            GO TO 999
          ENDIF
          R = R + THCEL ( J )
          CALL GSPOS(DIVSEC(I,J),1,DFSNAM(I), R,0.,0.,8200,'ONLY')
          CALL GSATT(DIVSEC(I,J),'SEEN',0)
C
C ****  Sub-divide the central cell into sub-cell 'DFT_',
C ****  containing one wire each.
C
          IF ( J .EQ. 2 ) THEN
            TDIV = 2. * THCEL(J ) / NDIV
            CALL GSDVT(DFTI(I),DIVSEC(I,J),TDIV,3,84,NDIV)
          ENDIF
          R = R + THCEL(J)
  300   CONTINUE
        JDRF = JDRF + 2
  200 CONTINUE
C
C ****  Define digitization and detector sets
C
      IF ( DCDC .GE. 2 ) CALL DETCDC
C
C **** Now order the volumes to speed things up
C
      CALL GSORD('CDC ',4)
      CALL GSORD('DRFT',3)
      CALL GSORD('DFS1',1)
      CALL GSORD('DFS2',1)
      CALL GSORD('DFS3',1)
      CALL GSORD('DFS4',1)
C
  999 CONTINUE
  500 FORMAT ( '****** GEOCDC : Problem in GSVOLU for ', A8)
      RETURN
      END
