      SUBROUTINE OH_GEOM(NAME,MOTHER,MATNO,CRACK,RIN,ROUT,DELZ,ZLO,ZHI,
     &  IRTOFF,IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Works out parameters for OH semi-Trap.
C-
C-   Inputs  : SRCP Survey parameters in CGS
C-             NAME = GEANT name of OH module
C-             MOTHER = Mother volume this hangs from
C-             MATNO = Material number
C-             CRACK = Width of crack in cm between modules
C-             RIN = Radius of cylinder tangential to inner
C-             surface of module
C-             ROUT = Radius of cylinder passing through outer
C-             apex of module
C-             DELZ = Half width of module in Z direction.
C-             ZLO = Z co-ordinate of center of inner face in the MOTHER
C-             VOLUME co-ordinate system
C-             ZHI = Z co-ordinate of center of outer face in the MOTHER
C-             VOLUME co-ordinate system
C-             IRTOFF = First Rotation matrix number
C-             IFL = 1 , PUT out 16 copies of Modules.
C-             IFL = 2 , Put out 1 copy hanging off the appropriate Mother
C-
C-   Outputs : SRCP GEANT parameters for EC OH module
C-   Controls: None
C-
C-   Created  9-NOV-88   Rajendran Raja
C-   Modified 20-Jun-1989 N.A. Graf Put in mother volume OCMV
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NPAR,IROT,NSEG,NAME,MOTHER,MATNO,NLN,IRTOFF
      INTEGER IFL
      REAL THET,CPDP,APBP,ANG
      REAL CRACK,RIN,ROUT,ZLO,ZHI,ZCEN,DELZ,XX,YY,ZZ
      CHARACTER*25 ARNAME
      CHARACTER*10 ARNAM
      DATA ARNAM/'\ARRAY EC_'/
      CHARACTER*14 ARNAM1
      CHARACTER*4 NAMEC
      CHARACTER*4 ACTION
      REAL ECCH(6)
      REAL BMPIP(50)
      INTEGER IBMPIP(50)
      EQUIVALENCE (BMPIP,IBMPIP)
      INTEGER IZ,IZZ,NAME1
C
      COMPLEX CENT,CENTR,AI,ZROT
      DATA AI/(0.0,1.0)/
      REAL PI,PAR(11),CTHET,STHET,TTHET,RAD
      INTEGER ICOPY,ICPY2,ICPY3,KK
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        IZ = 4   !IZ=5 IS THE FIRST NAME
        IZZ = 0
        FIRST = .FALSE.
        CALL GTSRCP('OH_PHI_SEGMENTS',NSEG,1)
        WRITE(20,21)NSEG
   21   FORMAT(' EC_OH_SEGMENTS  ',I2,'       !Number of Phi modules')
        CALL GTSRCP('PI',PI,1)
        THET = PI/NSEG   !HALF angle in radians . = 11.25degrees
        STHET = SIN(THET)
        CTHET = COS(THET)
        TTHET = STHET/CTHET
        CALL GTSRCP('MAIN_RING_BEAM_PIPE(1)',BMPIP(1),1)
      ENDIF
C
      NPAR = 11

      ECCH(1) = RIN
      ECCH(2) = ROUT
      ECCH(3) = DELZ
      ECCH(4) = ZLO
      ECCH(5) = ZHI
      ECCH(6) = 0.0 !NO TWIST
      ZCEN = 0.5*(ZLO+ZHI)
      CALL GETRAP(ECCH,CRACK,NSEG,PAR,RAD)
      CENT = RAD*CEXP(-AI*THET)  !Stagger
C
      IF ( IFL.EQ.1 ) THEN
        ICPY2 = 0
        DO 100 ICOPY = 1,NSEG
          IROT = IRTOFF+ICOPY-1
          ANG = (ICOPY)*2.0*PI/NSEG
          ZROT = CEXP(AI*ANG)
          CENTR = CENT*ZROT
          XX = REAL(CENTR)
          YY = AIMAG(CENTR)
          ACTION = 'POS '
          CALL UHTOC(NAME,4,NAMEC,4)
          ARNAM1 = ARNAM//NAMEC
          CALL STRINT(ARNAM1,ICOPY,ARNAME,NLN)
          NAME1 = NAME  !Save old name
          IF(ICOPY.EQ.IBMPIP(7).AND.IZZ.LT.2)THEN  !Only do for Modules
C                                        ! not massless gaps.
            IZ = IZ+1    !Putting Main Ring Beam pipe in. Array name not
C                        ! changed. Only Geant Volume changed.
            IZZ = IZZ+1
            ACTION = 'POS '
            NAME = IBMPIP(IZ)  !CHANGE THE NAME OF THE MODULE CONTAINING
C                              ! BEAM PIPE
            BMPIP(8+2*(IZZ-1))= XX
            BMPIP(8+2*(IZZ-1)+1)= YY   !STORING AWAY
            ICPY3 = 1
            IF(IZZ.EQ.2)
     &        CALL GTSRCP('MAIN_RING_BEAM_PIPE(1)',BMPIP(1),-1)
C SAVING AWAY
            ELSE
              ICPY2 = ICPY2+1
              ICPY3 = ICPY2
          ENDIF
          WRITE(20,1)ARNAME,NAME,MATNO,
     &        MOTHER,ACTION,
     &        IROT,ICPY3,XX,YY,ZCEN,NPAR,
     &        (PAR(KK),KK=1,NPAR)
          NAME = NAME1   !restore name
  100   CONTINUE
    1   FORMAT(1X,A20/,
     &  2X,'''',A4,'''',5X,'''TRAP''',5X,I2,5X,
     &  '''',A4,'''',5X,'''',A4,'''',/,
     &  I7,2X,I5,3F10.4,2X,I5,/,
     &  2X,6F10.4,/,5F10.4,/
     &  ' \END')
      ELSE
C hang off 1 copy from Mother volume .
        IROT = IRTOFF
        ICPY2 = 1
        XX = 0.0
        YY = ZCEN
        ZZ = 0.0
        ACTION = 'POS '
        CALL UHTOC(MOTHER,4,NAMEC,4)
        ARNAM1 = ARNAM//NAMEC
        CALL UHTOC(NAME,4,NAMEC,4)
        ARNAME = ARNAM1//NAMEC
        WRITE(20,1)ARNAME,NAME,MATNO,MOTHER,ACTION,
     &        IROT,ICPY2,XX,YY,ZZ,NPAR,
     &        (PAR(KK),KK=1,NPAR)
  500   CONTINUE
      ENDIF
      END
