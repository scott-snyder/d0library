      SUBROUTINE MH_GEOM(NAME,MOTHER,MATNO,CRACK,RIN,ROUT,DELZ,ZCEN,
     &  IRTOFF,IFL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Works out parameters for MH semi-Trap.
C-
C-   Inputs  : SRCP Survey parameters in CGS
C-             NAME(2) = GEANT names of the two trap halves of the module
C-             MOTHER = Mother volume this hangs from
C-             MATNO = Material number
C-             CRACK = Width of crack in cm between modules
C-             RIN = Radius of cylinder tangential to inner
C-             surface of module
C-             ROUT = Radius of cylinder passing through outer
C-             apex of module
C-             DELZ = Half width of module in Z direction.
C-             ZCEN = Z co-ordinate of center of module IN ITS MOTHER VOLUME
C-             IRTOFF = First Rotation matrix number
C-             IFL = 1 , PUT out 16 copies of Modules.
C-             IFL = 2 , Put out 1 copy hanging off the appropriate Mother
C-
C-   Outputs : SRCP GEANT parameters for EC MH module
C-   Controls: None
C-
C-   Created  25-OCT-1988   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NPAR,IROT,ITYP,NSEG,NAME(2),MOTHER(2),MATNO,NLN,IRTOFF
      INTEGER IFL
      REAL THET,CPDP,APBP,ANG
      REAL CRACK,RIN,ROUT,ZCEN,DELZ,XX,YY,ZZ,YY1(2)
      CHARACTER*25 ARNAME(2)
      CHARACTER*10 ARNAM(2)
      DATA ARNAM/'\ARRAY EC_','\ARRAY EC_'/
      CHARACTER*14 ARNAM1
      CHARACTER*4 NAMEC
      CHARACTER*4 ACTION
C
      COMPLEX CENT(2),CENTR,AI,ZROT
      DATA AI/(0.0,1.0)/
      REAL PI,PAR(4,2),CTHET,STHET,TTHET
      REAL XXP,XPCP,XPEP,BPEP,XPDP,XPBP,BPCP,XPAP,XPFP,XPGP
      INTEGER ICOPY,ICPY2,KK
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL GTSRCP('MH_PHI_SEGMENTS',NSEG,1)
        WRITE(20,21)NSEG
   21   FORMAT(' EC_MH_SEGMENTS  ',I2,'       !Number of Phi modules')
        CALL GTSRCP('PI',PI,1)
        THET = PI/NSEG   !HALF angle in radians . = 11.25degrees
        STHET = SIN(THET)
        CTHET = COS(THET)
        TTHET = STHET/CTHET
      ENDIF
C
C The geometry is to cut the D0 MH shape into two TRD1's.
C The center of rotation X. A is the Apex of the D0 shape. XA = Rout.
C XDE is a radius defining the edge of the D0 Shape.
C XCA is the radius to the apex. C is the point at which
C the circle of radius RIN is tangential to module.
C Due to crack, all points shift to their prime values. i.e
C X goes to XP, E to EP etc
C
      XXP = CRACK/STHET   !Shift in center due to Crack. Xp is the
C                         ! shifted center
      XPEP = ROUT*CTHET-CRACK/TTHET-CRACK*TTHET  !XP to EP distance.
      BPEP = XPEP*STHET
      XPCP = RIN +CRACK-CRACK/STHET
      CPDP = XPCP*TTHET
      XPBP = XPEP*CTHET
      BPCP = XPBP-XPCP
      XPFP = (XPBP+XPCP)/2.0
C
      XPAP = XPEP/CTHET
      APBP = XPAP-XPBP
      XPGP = (XPAP+XPBP)/2.0
C
      PAR(1,1) = CPDP
      PAR(2,1) = BPEP
      PAR(3,1) = DELZ
      PAR(4,1) = BPCP/2.0
      CENT(1) = CMPLX((XPFP+XXP),0.0)   !Position of center.
C
      PAR(1,2) = BPEP
      PAR(2,2) = 0.0
      PAR(3,2) = DELZ
      PAR(4,2) = APBP/2.0
      CENT(2) = CMPLX((XPGP+XXP),0.0)   !Position of center.
C
      NPAR = 4
      IF ( IFL.EQ.1 ) THEN
        DO 100 ITYP = 1,2   !TWO TRD1'S
          IF(NAME(ITYP).EQ.0)GO TO 100   !TO DO MASSLESS GAPS.
          DO 150 ICOPY = 1,NSEG
            ICPY2=ICOPY
            IROT = IRTOFF+ICOPY-1
            ANG = (ICOPY-1)*2.0*PI/NSEG
            ZROT = CEXP(AI*ANG)
            CENTR = CENT(ITYP)*ZROT
            XX = REAL(CENTR)
            YY = AIMAG(CENTR)
            ACTION = 'POS '
            CALL UHTOC(NAME(ITYP),4,NAMEC,4)
            ARNAM1 = ARNAM(ITYP)//NAMEC
            CALL STRINT(ARNAM1,ICPY2,ARNAME(ITYP),NLN)
            WRITE(20,1)ARNAME(ITYP),NAME(ITYP),MATNO,
     &        MOTHER(ITYP),ACTION,
     &        IROT,ICPY2,XX,YY,ZCEN,NPAR,
     &        (PAR(KK,ITYP),KK=1,NPAR)
            IF(ICOPY.EQ.1)THEN
              YY1(ITYP) = REAL(CENT(ITYP))  !Storing away for Zdivisions
C                                        ! later
            ENDIF
  150     CONTINUE
  100   CONTINUE
    1   FORMAT(1X,A20/,
     &  2X,'''',A4,'''',5X,'''TRD1''',5X,I2,5X,
     &  '''',A4,'''',5X,'''',A4,'''',/,
     &  I7,2X,I5,3F10.4,2X,I5,/,
     &  2X,4F10.4,/,
     &  ' \END')
      ELSE
C hang off 1 copy from Mother volume .
        IROT = IRTOFF
        XX = 0.0
        YY = ZCEN
        ACTION = 'POSP'
        DO 500 ITYP =1,2
          ICPY2 = 1                     ! Volume names are now different
          ZZ = REAL(CENT(ITYP))-YY1(ITYP) !Now in correct relative position
          CALL UHTOC(MOTHER(ITYP),4,NAMEC,4)
          ARNAM1 = ARNAM(ITYP)//NAMEC
          CALL UHTOC(NAME(ITYP),4,NAMEC,4)
          ARNAME(ITYP) = ARNAM1//NAMEC
          WRITE(20,1)ARNAME(ITYP),NAME(ITYP),MATNO,MOTHER(ITYP),ACTION,
     &        IROT,ICPY2,XX,YY,ZZ,NPAR,
     &        (PAR(KK,ITYP),KK=1,NPAR)
  500   CONTINUE
      ENDIF
      END
