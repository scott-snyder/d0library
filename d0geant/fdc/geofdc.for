      FUNCTION GEOFDC()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize the geometry for the Forward
C-                         Drift Chamber
C-
C-   Returned value  : TRUE
C-   Inputs  : None
C-   Outputs : None
C-   Controls: D0LOG.INC
C-
C    Geometry definition of the 'FDC ' detector volume
C    Define as a 'TUBE' positioned in NMCEN
C    Volume identifiers start with 'F'
C    Material, Media USE 90-99
C      and Rotations use 9000-9999
C
C    The structure of geometry for the forward drift chamber is
C    contained in the BLFxxx.FOR routines in the D0$STP$FDC area.
C
C-   Created  dd-mmm-198y   ????????????????
C-   Updated  26-JUL-1989   Jeffrey Bantly
C-   Updated  17-JUL-1989   Harrison B. Prosper
C-   Made into a pbd interface function; added call to DETFDC
C-   Updated   3-OCT-1989   Jeffrey Bantly  reorder for MC speedup
C-   Updated  12-AUG-1992   Robert E. Avery  Get FDC rotation from STP bank,
C-                  so as to use correct orientation for SFDC theta quads.
C
C---------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:CENMV.INC/LIST'
      INCLUDE 'D0$INC:ZEBSTP.INC/LIST'
      INCLUDE 'D0$INC:GCUNIT.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
C
C  Zebra Links
C
      INCLUDE 'D0$LINKS:IZFWAL.LINK/LIST'
      INCLUDE 'D0$LINKS:IZFWTA.LINK/LIST'
      INCLUDE 'D0$LINKS:IZFWTB.LINK/LIST'
      INCLUDE 'D0$LINKS:IZFWPH.LINK/LIST'
      INCLUDE 'D0$LINKS:IZFMAT.LINK/LIST'
      INCLUDE 'D0$LINKS:IZFDRT.LINK/LIST'
      INCLUDE 'D0$LINKS:IZFDTA.LINK/LIST'
      INCLUDE 'D0$LINKS:IZFDTB.LINK/LIST'
      INCLUDE 'D0$LINKS:IZFDPH.LINK/LIST'
      INTEGER LFMAT, LFWAL, LFDRT, LFDUM
      INTEGER LFWTA, LFWTB, LFWPH
      INTEGER LFDTA, LFDTB, LFDPH
C
C  Local Variables
C
      LOGICAL GEOFDC
C
      INTEGER IVOLU
      INTEGER I, J, K, XDIR, YDIR, ROT, MAT
      INTEGER ROT1,ROT2
      INTEGER IMED, IWED, NUM, NUMSUB
      REAL ATW, ATN, DEN, RDL, ABL, SIZE(5), POS(3)
      INTEGER KVLM, KVPAR, KVTYP, KVNT(3),KVSP(3)
      INTEGER KVNSEN, KVNWIR, KVNVOL, KVNUM
      INTEGER GZFGEH
      REAL KVWZ(3,16), KVWHT(3,16), KVWZC(3,16)
      REAL KVTAYP(8),KVTAYS(8),KVPHYP(16)
      REAL KVTBYP(8),KVTBYS(8)
      REAL ZOFSET, ANG, THIC, RMIN, R, Z
      CHARACTER*4 NAM,NAMOT,SUBNAM,TYP(10),TOPNAM
      CHARACTER*80 VARMSG
      CHARACTER*4 NAMEC
C
C------------------------------------------------------------------------------
C
      GEOFDC = .TRUE.
      IF ( DFDC .LT. 1 ) GOTO 999
C
C------------------------------------------------------------------------------
C                                                                             |
C  Define rotations                                                           |
C                                                                             |
C  9001 = global coordinate;              X=outside, Y=up, Z=proton direction |
C  9002 = mirror image to Y-X plane;      X=outside, Y=up, Z=pbar direction   |
C  9003 = mirror image to X-Z plane;            X=outside, Y=down,   Z=proton |
C  9004 = rotation of 90 degrees about Z axis;  X=up,      Y=inside, Z=proton |
C  9005-9006 = rotations for oddly fitted parts (tabs)                        |
C  9007 = rotation of 45 degrees about Z-axis;  X=out&up,  Y=up&in,  Z=pbar   |
C  9008 = rotation of 180 degrees about Z-axis; X=inside,  Y=down,   Z=proton |
C  9009 = rotation of 180 degrees about Y-axis; X=inside,  Y=up,     Z=pbar   |
C                                                                             |
C------------------------------------------------------------------------------
C
      CALL GSROTM(9001,90.,0.,90.,90.,0.,0.)
      CALL GSROTM(9002,90.,0.,90.,90.,180.,0.)
      CALL GSROTM(9003,90.,0.,90.,270.,0.,0.)
      CALL GSROTM(9004,90.,90.,90.,180.,0.,0.)
      CALL GSROTM(9005,90.,90.,0.,0.,90.,180.)
      CALL GSROTM(9006,90.,0.,0.,0.,90.,270.)
      CALL GSROTM(9007,90.,45.,90.,135.,180.,0.)
      CALL GSROTM(9008,90.,180.,90.,270.,0.,0.)
      CALL GSROTM(9009,90.,180.,90.,90.,180.,0.)
C
C  Define media.
C
      LFGEH = GZFGEH()
      LFMAT = LC ( LFGEH - IZFMAT )
      IF ( LFMAT .LE. 0 ) THEN
        WRITE ( VARMSG,10)
   10   FORMAT(' ** GEOFDC ** Bank FMAT doesn t exist:',
     &               ' can not define material in FDC')
        CALL ERRMSG('FDC-NO FMAT','GEOFDC',VARMSG,'F')
      ENDIF
      IMED = IC ( LFMAT + 1 )
      IWED = IC ( LFMAT + 2 )
      LFMAT = LFMAT + 2
      DO 400 I = 1, IMED
        NUM = IC ( LFMAT + 1 )
        ATW = C ( LFMAT + 7 )
        ATN = C ( LFMAT + 8 )
        DEN = C ( LFMAT + 9 )
        RDL = C ( LFMAT + 10)
        ABL = C ( LFMAT + 11)
        CALL UHTOC(IC(LFMAT+2),4,NAMEC,4)
        CALL GSMATE ( NUM, NAMEC, ATW, ATN,
     &                   DEN, RDL, ABL, 0, 0 )
        LFMAT = LFMAT + IWED
  400 CONTINUE
C
C---------------------------------------------------------------------------
C  Define material and media                                               |
C  Store the parameters of the tracking medium in the data structure       |
C  JTMED                                                                   |
C---------------------------------------------------------------------------
C
      CALL GSTMED(91,'VACUUM$',      16,0,0,0.,90.,1.,1.,.05,0.2,0,0)
      CALL GSTMED(92,'AIR$',         15,0,0,0.,90.,1.,1.,.05,0.2,0,0)
      CALL GSTMED(93,'ALUMINIUM$',    9,0,0,0.,90.,1.,1.,.05,0.2,0,0)
      CALL GSTMED(94,'HONEYCOMB$',   94,0,0,0.,90.,1.,1.,.05,0.2,0,0)
      CALL GSTMED(95,'ROHACELL$',    95,0,0,0.,90.,1.,1.,.05,0.2,0,0)
      CALL GSTMED(96,'G10$',         96,0,0,0.,90.,1.,1.,.05,0.2,0,0)
      CALL GSTMED(97,'FDC_GAS$',     97,1,0,0.,90.,1.,1.,.05,0.2,0,0)
      CALL GSTMED(98,'INTERNAL_CABLING$',
     &                               98,0,0,0.,90.,1.,1.,.05,0.2,0,0)
C
C--------------------------------------------------------------------------
C  Define and position the passive parts                                  |
C                                                                         |
C  Volume definition                                                      |
C          GSVOLU sets up a sub-volume of TUBS or BOX type of certain     |
C                 material or media of specific size with a certain       |
C                 number of parameters and returns the system volume      |
C                                                                         |
C  Positioning the pieces                                                 |
C          GSPOS positions a piece, the nth one, inside a mother volume   |
C                centered at mother volume position X, Y, Z with a        |
C                specified rotation and an overlap option if set          |
C--------------------------------------------------------------------------
C
C
C  Define and position FDC volume : Information is taken from the bank FGEH
C
      IF ( LFGEH .LT. 0 ) THEN
        WRITE ( VARMSG,20)
   20   FORMAT(' ** GEOFDC ** Bank FGEH doesn t exist:',
     &               ' can not define geometry in FDC')
        CALL ERRMSG('FDC-NO FGEH','GEOFDC',VARMSG,'F')
      ENDIF
C
      SIZE(1) = C ( LFGEH + 3 )
      SIZE(2) = C ( LFGEH + 4 )
      SIZE(3) = C ( LFGEH + 5 )
      POS(1)  = C ( LFGEH + 6 )
      POS(2)  = C ( LFGEH + 7 )
      POS(3)  = C ( LFGEH + 8 )
      ROT1 = IC ( LFGEH + 9 )
      ROT2 = IC ( LFGEH + 10 )
      IF  ( (ROT1.LT.9000).OR.(ROT1.GT.9999) 
     &  .OR.(ROT2.LT.9000).OR.(ROT2.GT.9999) ) THEN
        IC ( LFGEH + 9 )  = 9001
        IC ( LFGEH + 10 ) = 9002
        ROT1 = IC ( LFGEH + 9 )
        ROT2 = IC ( LFGEH + 10 )
      ENDIF
      NAM     = 'FDC '
C
      CALL GSVOLU ( NAM, 'TUBE', 91, SIZE(1), 3, IVOLU )
      IF ( IVOLU .LE. 0 ) THEN
        WRITE ( LOUT, 500) NAM
        GO TO 999
      ENDIF
      CALL UHTOC(NMCEN,4,NAMEC,4)
      CALL GSPOS ( NAM,1,NAMEC,POS(1),POS(2),(-POS(3)),ROT1,'ONLY')
      CALL GSPOS ( NAM,2,NAMEC,POS(1),POS(2),POS(3),ROT2,'ONLY')
      CALL GSATT(NAM,'SEEN',0)
C
C  Define and position Theta and Phi in an FDC volume : use bank FWAL
C
      LFWAL = LC ( LFGEH - IZFWAL )
      IF ( LFWAL .LE. 0 ) THEN
        WRITE ( VARMSG,30)
   30   FORMAT(' ** GEOFDC ** Bank FWAL doesn t exist:',
     &               ' can not define geometry in FDC')
        CALL ERRMSG('FDC-NO FWAL','GEOFDC',VARMSG,'F')
      ENDIF
      KVLM  = IC ( LFWAL + 1 )
      KVPAR = IC ( LFWAL + 2 )
      LFWAL = LFWAL + 2
      DO I = 1, KVLM
        CALL UHTOC ( C(LFWAL+1),4,NAM,4)
        CALL UHTOC ( C(LFWAL+11),4,NAMOT,4)
        SIZE(1) = C ( LFWAL + 2 )
        SIZE(2) = C ( LFWAL + 3 )
        SIZE(3) = C ( LFWAL + 4 )
        SIZE(4) = C ( LFWAL + 5 )
        SIZE(5) = C ( LFWAL + 6 )
        POS(1)  = C ( LFWAL + 7 )
        POS(2)  = C ( LFWAL + 8 )
        POS(3)  = C ( LFWAL + 9 )
        ROT     = IC ( LFWAL + 10 )
        MAT     = 91
        IF ( I .NE. 2 ) CALL GSVOLU ( NAM,'TUBE',MAT,SIZE(1),3,IVOLU )
        IF ( IVOLU .LE. 0 ) THEN
          WRITE ( LOUT, 500) NAM
          GO TO 999
        ENDIF
        IF ( I .NE. 2 ) THEN
          CALL GSPOS( NAM,1,NAMOT,POS(1),POS(2),POS(3),ROT,'ONLY')
        ELSE
          CALL GSPOS( NAM,2,NAMOT,POS(1),POS(2),POS(3),ROT,'ONLY')
        ENDIF
        CALL GSATT(NAM,'SEEN',0)
        LFWAL = LFWAL + KVPAR
        POS(1) = 0.0
        POS(2) = 0.0
        POS(3) = 0.0
        IF ( I .EQ. 1 ) THEN
          CALL GSVOLU ( 'FWTA', 'TUBS', MAT, SIZE(1), 5, IVOLU )
          CALL GSPOS( 'FWTA',1,NAM,POS(1),POS(2),POS(3),9001,'MANY')
          CALL GSPOS( 'FWTA',2,NAM,POS(1),POS(2),POS(3),9008,'MANY')
          CALL GSATT( 'FWTA','SEEN',0)
        ELSE IF ( I .EQ. 2 ) THEN
          CALL GSVOLU ( 'FWTB', 'TUBS', MAT, SIZE(1), 5, IVOLU )
          CALL GSPOS( 'FWTB',1,NAM,POS(1),POS(2),POS(3),9001,'MANY')
          CALL GSPOS( 'FWTB',2,NAM,POS(1),POS(2),POS(3),9008,'MANY')
          CALL GSATT( 'FWTB','SEEN',0)
        ELSE IF ( I .EQ. 3 ) THEN
C          CALL GSVOLU ( 'FDPH', 'TUBE', MAT, SIZE(1), 3, IVOLU )
C          CALL GSPOS( 'FDPH',1,NAM,POS(1),POS(2),POS(3),ROT,'ONLY')
C          CALL GSATT( 'FDPH','SEEN',0)
        ENDIF
      END DO
C
C  Define and position Theta and Phi components in their respective
C  volumes :  use banks FWTA,FWTB,FWPH
C
      LFWAL = LC ( LFGEH - IZFWAL )
      DO K = 1,3
        IF ( K .EQ. 1 ) THEN
          LFDUM   = LC ( LFWAL - IZFWTA )
          ZOFSET = 5.05
        ELSEIF ( K .EQ. 2 ) THEN
          LFDUM = LC ( LFWAL - IZFWTB )
          ZOFSET = 5.05
        ELSE
          LFDUM = LC ( LFWAL - IZFWPH )
          ZOFSET = -8.10
        ENDIF
        IF ( LFDUM .LE. 0 ) THEN
          WRITE ( VARMSG,40)
   40     FORMAT(' ** GEOFDC ** Bank FDUM doesn t exist:',
     &               ' can not define geometry in FDC')
          CALL ERRMSG('FDC-NO FDUM','GEOFDC',VARMSG,'F')
        ENDIF
        KVLM  = IC ( LFDUM + 1 )
        KVPAR = IC ( LFDUM + 2 )
        KVTYP = IC ( LFDUM + 3 )
        LFDUM = LFDUM + 3
        DO I = 1, KVTYP
          CALL UHTOC ( C(LFDUM+1),4,TYP(I),4)
          KVNT(I)  = IC ( LFDUM + 2 )
          KVSP(I)  = IC ( LFDUM + 3 )
          LFDUM = LFDUM + 3
        END DO
        DO I = 1, KVTYP
          DO J = 1, KVNT(I)
            IF ( I .EQ. 1 ) THEN
              CALL UHTOC ( C(LFDUM+1),4,NAM,4)
              CALL UHTOC ( C(LFDUM+9),4,NAMOT,4)
              SIZE(1) = C ( LFDUM + 2 )
              SIZE(2) = C ( LFDUM + 3 )
              SIZE(3) = C ( LFDUM + 4 )
              POS(1)  = C ( LFDUM + 5 )
              POS(2)  = C ( LFDUM + 6 )
              POS(3)  = C ( LFDUM + 7 ) + ZOFSET
              MAT     = IC ( LFDUM + 8 )
              ROT = 9001
              CALL GSVOLU ( NAM, TYP(I), MAT, SIZE(1), 3, IVOLU )
            ELSE
              CALL UHTOC ( C(LFDUM+1),4,NAM,4)
              CALL UHTOC ( C(LFDUM+9),4,NAMOT,4)
              SIZE(1) = C ( LFDUM + 2 )
              SIZE(2) = C ( LFDUM + 3 )
              SIZE(3) = 0.40
              SIZE(4) = C ( LFDUM + 4 )
              POS(1)  = C ( LFDUM + 5 )
              POS(2)  = C ( LFDUM + 6 )
              POS(3)  = C ( LFDUM + 7 ) + ZOFSET
              ROT     = IC ( LFDUM + 8 )
              MAT = 96
              CALL GSVOLU ( NAM, TYP(I), MAT, SIZE(1), 4, IVOLU )
            END IF
            IF ( IVOLU .LE. 0 ) THEN
              WRITE ( LOUT, 500) NAM
              GO TO 999
            ENDIF
            CALL GSPOS( NAM,1,NAMOT,POS(1),POS(2),POS(3),ROT,'ONLY')
            IF( SIZE(3) .LE. 0.0 ) THEN
              CALL GSATT(NAM,'SEEN',0)
            ENDIF
            LFDUM = LFDUM + KVPAR
          END DO
        END DO
      END DO
C
C-----------------------------------------------------------------------
C  Define and position the sensitive gas parts                         |
C  Sub-divide gas volumes at one sub-division per wire                 |
C                                                                      |
C  GSDVN creates sub-divisions of a volume into  a number              |
C        of sub-volumes in a specified direction                       |
C-----------------------------------------------------------------------
C
C  Define the volumes contained in FDRT
C
C
      LFDRT = LC ( LFGEH - IZFDRT )
      IF ( LFDRT .LE. 0 ) THEN
        WRITE ( VARMSG,50)
   50   FORMAT(' ** GEOFDC ** Bank FDRT doesn t exist:',
     &               ' can not define geometry in FDC')
        CALL ERRMSG('FDC-NO FDRT','GEOFDC',VARMSG,'F')
      ENDIF
      KVNUM  = IC ( LFDRT + 1 )
      DO K = 1, KVNUM
        IF ( K .EQ. 1 ) THEN
          LFDUM   = LC ( LFDRT - IZFDTA )
          ZOFSET = 5.05
        ELSEIF ( K .EQ. 2 ) THEN
          LFDUM = LC ( LFDRT - IZFDTB )
          ZOFSET = 5.05
        ELSE
          LFDUM = LC ( LFDRT - IZFDPH )
          ZOFSET = -8.10
        ENDIF
        IF ( LFDUM .LE. 0 ) THEN
          WRITE ( VARMSG,60)
   60     FORMAT(' ** GEOFDC ** Bank FDUM doesn t exist:',
     &               ' can not define geometry in FDC')
          CALL ERRMSG('FDC-NO FDUM','GEOFDC',VARMSG,'F')
        ENDIF
        KVNSEN = IC ( LFDUM + 1 )
        KVNWIR = IC ( LFDUM + 2 )
        KVPAR = IC ( LFDUM + 3 )
        KVNVOL = IC ( LFDUM + 4 )
        CALL UHTOC ( C(LFDUM+1),4,TYP(K),4)
        LFDUM = LFDUM + 5
        DO I = 1, KVNWIR
          KVWZ(K,I)   = IC ( LFDUM + I )
          KVWHT(K,I)  = IC ( LFDUM + KVNWIR + I )
          KVWZC(K,I)  = IC ( LFDUM + 2*KVNWIR + I )
        END DO
        LFDUM = LFDUM + 3*KVNWIR
        IF ( K .EQ. 1 ) THEN
          DO I = 1, KVNSEN
            KVTAYP(I) = C ( LFDUM + I )
          ENDDO
          LFDUM = LFDUM + KVNSEN
          DO I = 1, KVNWIR
            KVTAYS(I) = C ( LFDUM + I )
          ENDDO
          LFDUM = LFDUM + KVNWIR
        ELSEIF ( K .EQ. 2 ) THEN
          DO I = 1, KVNSEN
            KVTBYP(I) = C ( LFDUM + I )
          ENDDO
          LFDUM = LFDUM + KVNSEN
          DO I = 1, KVNWIR
            KVTBYS(I) = C ( LFDUM + I )
          ENDDO
          LFDUM = LFDUM + KVNWIR
        ELSE
          DO I = 1, KVNWIR
            KVPHYP(I) = C ( LFDUM + I )
          END DO
          LFDUM = LFDUM + KVNWIR
        ENDIF
        DO I = 1, KVNVOL
          CALL UHTOC ( C(LFDUM+1),4,NAM,4)
          CALL UHTOC ( C(LFDUM+9),4,NAMOT,4)
          SIZE(1) = C ( LFDUM + 2 )
          SIZE(2) = C ( LFDUM + 3 )
          SIZE(3) = C ( LFDUM + 4 )
          POS(1)  = C ( LFDUM + 5 )
          POS(2)  = C ( LFDUM + 6 )
          POS(3)  = C ( LFDUM + 7 ) + ZOFSET
          CALL UHTOC ( C(LFDUM+8),4,SUBNAM,4)
          MAT     = 97
          ROT     = 9002
          IF ( K .LE. 2 ) THEN
            TYP(K) = 'BOX '
            CALL GSVOLU ( NAM, TYP(K), MAT, SIZE(1), 3, IVOLU )
            IF ( IVOLU .LE. 0 ) THEN
              WRITE ( LOUT, 500) NAM
              GO TO 999
            ENDIF
            CALL GSPOS( NAM,1,NAMOT,POS(1),POS(2),POS(3),ROT,'ONLY')
            CALL GSDVN(SUBNAM,NAM,KVNWIR,3)
            CALL GSATT(SUBNAM,'SEEN',0)
          ELSE
            TYP(K) = 'TUBS'
            SIZE(4) = 60.*(FLOAT(I)-1.)
            SIZE(5) = 60.*FLOAT(I)
            CALL GSVOLU ( NAMOT, TYP(K), MAT, SIZE(1), 5, IVOLU )
            IF ( IVOLU .LE. 0 ) THEN
              WRITE ( LOUT, 500) NAM
              GO TO 999
            ENDIF
            CALL GSPOS(NAMOT,1,'FDPH',POS(1),POS(2),POS(3),ROT,'ONLY')
            CALL GSDVN(NAM,NAMOT,KVNSEN,2)
            CALL GSDVN(SUBNAM,NAM,KVNWIR,3)
            CALL GSATT(NAMOT,'SEEN',0)
            CALL GSATT(SUBNAM,'SEEN',0)
          ENDIF
          LFDUM = LFDUM + KVPAR
        END DO
      END DO
C
C---------------------------------------------------------------------
C  Order the search pattern for some of the volumes and make some of
C  them unseen.
C---------------------------------------------------------------------
C
      CALL GSORD('FWTA',2)
      CALL GSORD('FWTB',1)
      CALL GSORD('FDPH',3)
      DO K = 1, 2
        IF ( K.EQ.1 ) LFDUM = LC( LFWAL - IZFWTA )
        IF ( K.EQ.2 ) LFDUM = LC( LFWAL - IZFWTB )
        KVPAR = IC( LFDUM+2 )
        KVTYP = IC( LFDUM+3 )
        LFDUM = LFDUM + 3 + KVTYP*3
        DO I=1,13                       ! sector & intercell mother vols
          CALL UHTOC( C(LFDUM+1),4,NAM,4 )
          CALL GSORD(NAM,3)
          CALL GSATT(NAM,'SEEN',0)
          LFDUM = LFDUM + KVPAR
        ENDDO
      ENDDO
C
C ****  Setup digitization and sets
C
      IF ( DFDC .GE. 2 ) CALL DETFDC
C
C
C-----------------
C  Done          |
C-----------------
C
  999 CONTINUE
  500 FORMAT ( '****** GEOFDC : Problem in GSVOLU for ', A8)
      RETURN
      END
