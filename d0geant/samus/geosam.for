C+
      LOGICAL FUNCTION GEOSAM ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Defines SAMUS geometry. Now it includes two
C-                         magnetic toroids, two collimators, two low beta
C-                         quadrupoles, part of the beam-pipe (outside
C-                         calorimeters) and six stations (+A,-A,
C-   +B,-B,+C,-C). The stations have three planes (X,Y,U) which are
C-   devided into two sections. Each section can contain from 128 to
C-   189 drift tubes. In whole there are 5308 tubes in SAMUS. But there
C-   are only 129 different tubes' types. Only these types are described
C-   by GSVOLU. Each SAMUS tube has a unique number NR ( when calling
C-   GSPOS ) which equals:
C-   NR = (Nstation-1)*1200+(Nsection-1)*200+Ntube, where
C-   Nstation - SAMUS station #,
C-   Nsection - SAMUS section #,
C-   Ntube - drift tube # in section.
C-
C-   Inputs  : None
C-   Outputs : None
C-
C-   Created  27-SEP-1990   A.Kiryunin
C-   Updated  18-OCT-1990   A.Kiryunin:  New set-up of SAMUS geometry
C-   Updated  12-MAR-1991   Andrei Kiryunin:  Include CALL GSMATE for
C-                          toroid material.
C-   Updated  01-APR-1991   Andrei Kiryunin:  Include quadrupoles and
C-                          SAMUS collimator description.
C-   Updated  21-APR-1991   Andrei Kiryunin:  Reading geometry
C-                          through RCP files .
C-   Updated  11-mar-1992   S. Abachi  SD0(3) used to flag creation of
C-                          MURECO_GSAVE.DAT by modifying this code.
C-   Updated  31-MAR-1992   K. Wyatt Merritt  Restrict physics
C-                          processes in lead and tungsten for the
C-                          collimators to reduce CPU time.
C-   Updated  19-NOV-1992   Alexander Efimov:  add Ailer angles for
C-                          station orientation.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:D0LOG.INC'
      INTEGER IVOL, NST, NSE, NTU, MST, MSE, NROT, NR
      INTEGER NTUBES, NTY,NTYPES
      INTEGER NTY_TUB(2), SHIFT
      INTEGER NPAR, T_MEDIUM, V_MEDIUM
      REAL    CTUB(3,2), VTUB(3,2), TUPAR(3), VOPAR(3)
      REAL    RIN, ROUT, DEND, DTUB
      CHARACTER*4 NAME, TUBE_NAME, VOLU_NAME, SHAPE
      INTEGER IFIELD
      INTEGER NBUFF,IBUFF(1)
      REAL FIELDM, TMAXFD, DMAXMS, DEEMAX, EPSIL, STMIN
      INTEGER IFI, IF_SAGTUB, SAGTUB
      CHARACTER*80 MSGSTR               ! Error message
      DATA NBUFF/0/, IBUFF(1)/0/
      DATA IFIELD/1/, FIELDM/20.0/
      DATA TMAXFD, DMAXMS, DEEMAX, EPSIL, STMIN /.5,2.,.1,.05,.05/
C
      GEOSAM = .TRUE.
      IF (DSAM .LE. 0) GOTO 999
C
C ****  Define GEANT rotation matrices
C
      CALL GSROTM (10000, 90.,  0., 90., 90.,  0.,  0.) ! No rotation
      CALL GSROTM (10005, 90.,  0.,180.,  0., 90., 90.) ! X-tubes
      CALL GSROTM (10006, 90., 90.,180.,  0., 90.,180.) ! Y-tubes
      CALL GSROTM (10007, 90.,315.,180.,  0., 90., 45.) ! U-tubes
C
C ****  Define toroid and collimator material
C
      CALL GSMATE (103,'TUNGSTEN FOR SAMUS$',183.85,74.00,19.30,0.35,
     &             10.300, NBUFF,IBUFF)
      CALL GSMATE (104,'LEAD FOR SAMUS$',207.19,82.00,11.35,0.56,
     &             18.500, NBUFF,IBUFF)
      CALL GSMATE (109,'IRON SAMUS TOROID$',55.850,26.00,7.870,1.760,
     &             17.100, NBUFF,IBUFF)
C
C ****  Define tracking media
C
      CALL GSTMED (100,'Air in SAMUS       $', 15, 0,
     &     0,   0.0,TMAXFD,DMAXMS,DEEMAX,EPSIL,STMIN,0,0)
      CALL GSTMED (101,'SAMUS tube gas     $', 15, 1,    ! Air now
     &     0,   0.0,TMAXFD,DMAXMS,DEEMAX,EPSIL,STMIN,0,0)
      CALL GSTMED (102,'Iron for SAMUS tube$', 10, 0,
     &     0,   0.0,TMAXFD,DMAXMS,DEEMAX,EPSIL,STMIN,0,0)
      CALL GSTMED (103,'Tungsten for SAMUS $',103, 0,
     &     0,   0.0,TMAXFD,DMAXMS,DEEMAX,EPSIL,STMIN,0,0)
      CALL GSTMED (104,'Lead for SAMUS     $',104, 0,
     &     0,   0.0,TMAXFD,DMAXMS,DEEMAX,EPSIL,STMIN,0,0)
      CALL GSTMED (109,'SAMUS iron toroids $',109,0,
     &IFIELD,FIELDM,TMAXFD,DMAXMS,DEEMAX,EPSIL,STMIN,0,0)
C
C ****  Reset physics processes in iron toroids and lead
C
      IF(SD0(3) .EQ. 1.0) GOTO 19   !See below
C
      CALL GSTPAR(103,'CUTGAM',0.2)
      CALL GSTPAR(103,'CUTELE',0.2)
      CALL GSTPAR(103,'CUTNEU',0.2)
      CALL GSTPAR(103,'CUTHAD',0.2)
      CALL GSTPAR(103,'PAIR',2.)
      CALL GSTPAR(103,'COMP',2.)
      CALL GSTPAR(103,'PHOT',2.)
      CALL GSTPAR(103,'PFIS',2.)
      CALL GSTPAR(103,'DRAY',2.)
      CALL GSTPAR(103,'ANNI',2.)
      CALL GSTPAR(103,'BREM',2.)
      CALL GSTPAR(103,'HADR',1.)
      CALL GSTPAR(103,'MUNU',1.)
      CALL GSTPAR(103,'DCAY',1.)
      CALL GSTPAR(103,'LOSS',2.)
      CALL GSTPAR(103,'MULS',2.)
C
      CALL GSTPAR(104,'CUTGAM',0.2)
      CALL GSTPAR(104,'CUTELE',0.2)
      CALL GSTPAR(104,'CUTNEU',0.2)
      CALL GSTPAR(104,'CUTHAD',0.2)
      CALL GSTPAR(104,'PAIR',2.)
      CALL GSTPAR(104,'COMP',2.)
      CALL GSTPAR(104,'PHOT',2.)
      CALL GSTPAR(104,'PFIS',2.)
      CALL GSTPAR(104,'DRAY',2.)
      CALL GSTPAR(104,'ANNI',2.)
      CALL GSTPAR(104,'BREM',2.)
      CALL GSTPAR(104,'HADR',1.)
      CALL GSTPAR(104,'MUNU',1.)
      CALL GSTPAR(104,'DCAY',1.)
      CALL GSTPAR(104,'LOSS',2.)
      CALL GSTPAR(104,'MULS',2.)
C
      CALL GSTPAR(109,'CUTGAM',0.2)
      CALL GSTPAR(109,'CUTELE',0.2)
      CALL GSTPAR(109,'CUTNEU',0.2)
      CALL GSTPAR(109,'CUTHAD',0.2)
      CALL GSTPAR(109,'PAIR',2.)
      CALL GSTPAR(109,'COMP',2.)
      CALL GSTPAR(109,'PHOT',2.)
      CALL GSTPAR(109,'PFIS',2.)
      CALL GSTPAR(109,'DRAY',2.)
      CALL GSTPAR(109,'ANNI',2.)
      CALL GSTPAR(109,'BREM',2.)
      CALL GSTPAR(109,'HADR',1.)
      CALL GSTPAR(109,'MUNU',1.)
      CALL GSTPAR(109,'DCAY',1.)
      CALL GSTPAR(109,'LOSS',2.)
      CALL GSTPAR(109,'MULS',2.)
C
   19 CONTINUE
C
C ****  Construct geometry for the SAMUS magnets
C
      CALL SAM_MA
C
C ****  Construct geometry for the collimators, quarupoles and 
C ****  beam-pipe in SAMUS region
C
      CALL SAM_BP
C
      IF(SD0(3) .EQ. 1.0) GOTO 20
C
C     ! SD0(3)=1 Should be chosen only when attempting
C     ! to create the STP file MURECO_GSAVE.DAT for
C     ! mureco package. In this case all PBD modules
C     ! are excluded and GSORD calls are not made.
C
C ****  Define number of tubes' types, their names and some constants
C
      CALL SAGCON (RIN,ROUT,DEND,T_MEDIUM,V_MEDIUM)
                                          ! Define constants for drift tubes
      CALL SAGTYP (NTYPES)                ! Define number of tubes' types
      CALL SANAME (1,TUBE_NAME,VOLU_NAME) ! Define all tubes' names
      NPAR = 3
      SHAPE = 'TUBE'
C
C ****  Loop over tubes' types
C
      DO NTY = 1,NTYPES
        CALL SANAME (NTY,TUBE_NAME,VOLU_NAME)
        CALL SAGTBL (NTY, DTUB)
        CALL SATUBE (DTUB,RIN,ROUT,DEND, TUPAR,VOPAR)
C       -- Define steel tube
        CALL GSVOLU( TUBE_NAME, SHAPE, T_MEDIUM, TUPAR, NPAR, IVOL )
        CALL GSATT ( TUBE_NAME, 'SEEN', 0 )
C       -- Define drift volume inside steel tube
        CALL GSVOLU( VOLU_NAME, SHAPE, V_MEDIUM, VOPAR, NPAR, IVOL )
        CALL GSATT ( VOLU_NAME, 'SEEN', 0 )
      END DO
C
C ****  Loop over SAMUS stations
C
      DO NST = 1,6
        CALL SASTAT (NST, NAME)           ! Define station GEANT volume
        MST = (NST - 1) * 1200
C
C ****  Loop over six sections in a station
C
        DO NSE = 1,6
          CALL SAGSEC (NST, NSE, NTUBES, NROT, SHIFT)
          NROT = 10004 + NROT
          MSE = (NSE - 1) * 200
C
C ****  Loop over tubes in section
C
          DO NTU = 1,NTUBES
            IF_SAGTUB = SAGTUB (NST, NSE, NTU, CTUB, VTUB, NTY_TUB)
            IF (IF_SAGTUB.LE.0) THEN
              MSGSTR = ' GEOSAM: bad SAMUS tube description '
              CALL INTMSG (MSGSTR)
              GOTO 999
            ELSE   
              DO IFI = 1, IF_SAGTUB
                CALL SANAME (NTY_TUB(IFI), TUBE_NAME, VOLU_NAME)
                IF (IFI .EQ. 1) THEN
                  NR = MST + MSE + NTU
                ELSE
                  NR = MST + MSE + NTU + SHIFT
                ENDIF
C               -- Place steel tube
                CALL GSPOS (TUBE_NAME, NR, NAME, CTUB(1,IFI),
     &                      CTUB(2,IFI), CTUB(3,IFI), NROT, 'ONLY')
C               -- Place drift volume inside steel tube
                CALL GSPOS (VOLU_NAME, NR, NAME, CTUB(1,IFI),
     &                      CTUB(2,IFI), CTUB(3,IFI), NROT, 'ONLY')
              END DO
            END IF
          END DO
        END DO
      END DO
C
C ****  Set up detector
C
      IF (DSAM.GE.2) CALL DETSAM
      GOTO 999
C
   20 CONTINUE
C
  999 RETURN
      END
