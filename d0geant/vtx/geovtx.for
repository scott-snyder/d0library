      FUNCTION GEOVTX()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-  Subroutine GEOVTX defines the 'VTX ' detector volume and its
C-  subvolumes.  'VTX ' is defined as a 'TUBE' positioned in NMCEN.
C-  Volume identifiers start with 'V'.
C-  Material, Media use 60-69 .
C-  Rotations use 6000-6999 .
C-  Units are centimeters.
C-  Coordinate system is cartesian, right-handed with protons
C-  along +z, y up, and x horizontal and out from the center of
C-  the storage ring.
C-
C-   Returned value  : TRUE
C-   Inputs  : None
C-   Outputs : None
C-   Controls: D0LOG.INC
C-
C-   Created   2-NOV-1985   T. Trippe
C-   Updated  21-OCT-1988   Ghita Rahal-Callot  : take data from the STP_file
C-   Updated  22-JUN-1989   Thomas G. Trippe  : New VTX geometry, D0 note #808
C-   Updated  17-JUL-1989   Harrison B. Prosper
C-   Made into pbd interface function. Added call to DETVTX.
C-   Updated  17-AUG-1989   N.A. Graf Add call to GSORD
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$INC:CENMV.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC'
      INCLUDE 'D0$LINKS:IZVWAL.LINK'
      INCLUDE 'D0$LINKS:IZVMAT.LINK'
      INCLUDE 'D0$LINKS:IZVRFT.LINK'
C
      LOGICAL GEOVTX
C
      INTEGER LVMAT, LVWAL, LVRFT
      INTEGER NSEC,IVOL, I, J
      INTEGER IMED, IWED, NUM
      INTEGER KVLM, KVPAR
C
      REAL ATW, ATN, DEN, RDL, ABL
      REAL X(4), XCENT, YCENT, ZCENT, RCEN, Z, PI, PHI0
C
      CHARACTER*4 NAM, NAMOT
      CHARACTER*4 VGVNAM(0:2), VSCNAM(0:2), VTRNAM(0:2), VCLNAM(0:2)
      CHARACTER*4 NAMEC
C
C----------------------------------------------------------------------
      DATA VGVNAM / 'VGV0', 'VGV1', 'VGV2' /
      DATA VSCNAM / 'VSC0', 'VSC1', 'VSC2' /
      DATA VTRNAM / 'VTR0', 'VTR1', 'VTR2' /
      DATA VCLNAM / 'VCL0', 'VCL1', 'VCL2' /
C----------------------------------------------------------------------
      GEOVTX = .TRUE.
      IF ( DVTX .LT. 1 ) GOTO 999
C
      PI = ACOS(-1.)
C
C  Define rotation matix,  6000 = Position of the VTX in the D0 frame
C                          6100 = Position of all the volumes in the VTX
C                                 frame
C                          6200 = Position of the cells compared to VTX
C
C  ********** caution : 6000 should depend later on the alignment
      CALL GSROTM(6000,90.,0.,90.,90.,0.,0.)
      CALL GSROTM(6100,90.,0.,90.,90.,0.,0.)
      CALL GSROTM(6200,90.,90.,0.,0.,90.,0.)
C
C
C  Define materials
C
C
      LVMAT = LC ( LVGEH - IZVMAT )
      IF ( LVMAT .LE. 0 ) THEN
        WRITE ( LOUT,*) ' routine GEOVTX: bank VMAT doesn''t exist:',
     +               ' impossible to define the material in the VTX'
        CALL EXIT(1)
      ENDIF
      IMED = IC ( LVMAT + 1 )
      IWED = IC ( LVMAT + 2 )
      LVMAT = LVMAT + 2
      DO 400 I = 1, IMED
        NUM = IC ( LVMAT + 1 )
        ATW = C ( LVMAT + 7 )
        ATN = C ( LVMAT + 8 )
        DEN = C ( LVMAT + 9 )
        RDL = C ( LVMAT + 10)
        ABL = C ( LVMAT + 11)
        CALL UHTOC( IC(LVMAT+2), 4, NAMEC,4)
        CALL GSMATE ( NUM, namec, ATW, ATN,
     +                   DEN, RDL, ABL, 0, 0 )
        LVMAT = LVMAT + IWED
  400 CONTINUE
C
C****  Define media.  Store the parameters of the tracking medium in
C****  the data structure JTMED.
C
      CALL GSTMED(60,'VTX WALL 0$' ,60,0,0,0.,0.,0.1,0.1,0.05,0.5,0,0)
      CALL GSTMED(61,'VTX WALL 1$' ,61,0,0,0.,0.,0.1,0.1,0.05,0.5,0,0)
      CALL GSTMED(62,'VTX WALL 2$' ,62,0,0,0.,0.,0.1,0.1,0.05,0.5,0,0)
      CALL GSTMED(63,'VTX WALL 3$' ,63,0,0,0.,0.,0.1,0.1,0.05,0.5,0,0)
      CALL GSTMED(64,'VTX END CAP 0$',64,0,0,0.,0.,0.1,0.1,0.05,0.5,0,0)
      CALL GSTMED(65,'VTX END CAP 1$',65,0,0,0.,0.,0.1,0.1,0.05,0.5,0,0)
      CALL GSTMED(66,'VTX END CAP 2$',66,0,0,0.,0.,0.1,0.1,0.05,0.5,0,0)
      CALL GSTMED(67,'VTX GAS, DEAD AREA$',
     +                                67,0,0,0.,0.,0.1,0.1,0.05,0.5,0,0)
      CALL GSTMED(68,'VTX GAS, SENSITIVE$',
     +                                67,1,0,0.,0.,0.1,0.1,0.05,0.5,0,0)
      CALL GSTMED(69,'VTX AIR,MOTHER VOL$',
     +                                15,0,0,0.,0.,0.1,0.1,0.05,0.5,0,0)
C-----------------------------------------------------------------------
C
C****  Define volumes : Information is taken from the bank VWAL.
C
      LVWAL = LC ( LVGEH - IZVWAL )
      IF ( LVWAL .LE. 0 ) THEN
        WRITE ( LOUT,*) ' routine GEOVTX: bank VWAL doesn''t exist:',
     +               ' impossible to define the geometry of the VTX'
        CALL EXIT(1)
      ENDIF
      KVLM  = IC ( LVWAL + 1 )
      KVPAR = IC ( LVWAL + 2 )
      LVWAL = LVWAL + 2
      XCENT = C ( LVGEH + 3 )
      YCENT = C ( LVGEH + 4 )
      ZCENT = C ( LVGEH + 5 )
      DO 100 I = 1, KVLM
        CALL UHTOC ( iC(LVWAL+1),4,NAM,4)
        CALL UHTOC ( iC(LVWAL+7),4,NAMOT,4)
        X(1) = C ( LVGEH + IC ( LVWAL + 2 ) )
        X(2) = C ( LVGEH + IC ( LVWAL + 3 ) )
        X(3) = C ( LVGEH + IC ( LVWAL + 5 ) )
        J = IC ( LVWAL + 4 )
        IF ( J .NE. 0 ) X(3) = ( X(3) - C(LVGEH+J)) / 2
        CALL GSVOLU ( NAM, 'TUBE', IC(LVWAL+6), X, 3, IVOL )
        IF ( IVOL .LE. 0 ) THEN
          WRITE ( LOUT, 500) NAM
          GO TO 999
        ENDIF
C
C ****  Position carbon fiber tubes and gas layers at origin of 'VTX '
C **** and position copies of end plates at both ends of gas layers.
C ****  For the volume VTX, the position depend on the alignment; the other
C ****  volumes refer to the VTX volume.
C
        IF ( NAM . EQ. 'VTX' ) THEN
          CALL UHTOC(NMCEN,4,NAMEC,4)
          CALL GSPOS (NAM,1,NAMEC,XCENT,YCENT,ZCENT,6000,'ONLY')
        ELSE
C
C ****  Do we need 1 or 2 copies of the volume ?
C
          IF ( IC(LVWAL+4) .EQ. 0 ) THEN
            CALL GSPOS (NAM,1,NAMOT, 0., 0., 0.,6100,'ONLY')
          ELSE
            Z = C(LVGEH + IC(LVWAL+4)) + C(LVGEH + IC(LVWAL+5))
            Z = Z / 2.
            CALL GSPOS (NAM,1,NAMOT, 0., 0., Z,6100,'ONLY')
            CALL GSPOS (NAM,2,NAMOT, 0., 0., -Z,6100,'ONLY')
          ENDIF
        ENDIF
        LVWAL = LVWAL + KVPAR
  100 CONTINUE
C
C
C----------------------------------------------------------------------
C
C  Level 3:  Subdivide gas layers into phi sectors.
C **** Make them "unseen" so they won't be drawn.
C
      LVRFT = LC ( LVGEH - IZVRFT )
      DO 300 I =  0, 2
        PHI0 = C(LVRFT + 7*I + 8) - C (LVRFT + 7*I + 6)
        NSEC = IC(LVRFT + 2 + 7*I)
        CALL GSDVN2 ( VSCNAM(I), VGVNAM(I), NSEC, 2, PHI0, 67)
        CALL GSATT  ( VSCNAM(I), 'SEEN', 0 )
C
C ****  Insert in each phi sector a trapezoidal sensitive volume
C
C
        RCEN = C(LVRFT+7+7*I)
        X(1) = ( RCEN - C(LVRFT+7*I+5))* TAN( PI / NSEC)
        X(2) = ( RCEN + C(LVRFT+7*I+5))* TAN( PI / NSEC)
        X(3) = C(LVGEH + 17 + 2*I)
        X(4) = C(LVRFT +  5 + 7*I)
        CALL GSVOLU ( VTRNAM(I), 'TRD1', 68, X, 4, IVOL )
        CALL GSPOS  ( VTRNAM(I), 1, VSCNAM(I),RCEN,0.,0.,6200,'ONLY')
C
C ****  Subdivide each trapezoid into cells containing one wire each
C **** Make the cells "unseen" so they won't be drawn
C
        CALL GSDVN( VCLNAM(I), VTRNAM(I), IC(LVRFT+3+7*I), 3)
        CALL GSATT( VCLNAM(I), 'SEEN', 0 )
  300 CONTINUE
C
C ****  Define detector sets and digitization
C
      IF ( DVTX .GE. 2 ) CALL DETVTX
C
C
C ****  Now order the volume to speed things up
C
      CALL GSORD('VTX ',4)
  999 CONTINUE
      RETURN
  500 FORMAT ( '****** GEOVTX : Problem in GSVOLU for ', A8)
      END

