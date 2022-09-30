      SUBROUTINE MXLV0
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Defines materials for LEVEL ZERO DETECTOR
C-                         Calls GSMIXT if DLAM = 0
C-                         else CALLS GSMATE with Absorption
C-                         Lengths we calculate
C-
C-   Inputs  : NONE
C-   Outputs : None
C-   Controls: None
C-
C-   CREATED  14-APR-1989   Chip Stewart  BASED ON MXCAL ROUTINE
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*32 NMSRCP
      CHARACTER*32 NMSRC1
      INTEGER LEN3
C
      INCLUDE 'D0$INC:D0LOG.INC'
C
      INTEGER NLMAT,NMIX,IMXNO,J,IPT,NLMATA
C
      REAL AA(10),ZZ(10),DENS,WMAT(10),TOTL,D,RL,AL
      REAL UB,TOTABS,TOTRAD,AEFF,ZEFF
C
      CHARACTER*12 MIXNAC
      INTEGER MIXNA1(3)
C  DEFINE MIXTURE DATA, NMIX TYPES
      INTEGER IMXMAT(10),I,NU
      REAL DL(10)
c      REAL DUM(10)
C
      INTEGER LMIX(200)
      REAL RMIX(200)
      EQUIVALENCE ( LMIX,RMIX )
C
      INTEGER ISVOL,IFIELD
      REAL FIELDM,TMAXFD,DMXMS,DEEMAX,EPSIL,STMIN
c      CHARACTER*4 NAMEC(3)
      CHARACTER*30 DUM
C
C----------------------------------------------------------------------
      DATA NMSRCP/'LV0_MIXTURES'/
C----------------------------------------------------------------------
C
C ****  Get data from SRCP bank which was selected prior to calling
C       this routine
C
      CALL ADDSTR(NMSRCP,'(1)',NMSRC1,LEN3)   !Makes it into array format
      CALL GTSRCP_i(NMSRC1,LMIX(1),1)
C
C ****  All general stuff is in bank SRCP_REST
C
      CALL GTSRCP_i('MXLV0_ISVOL',ISVOL,1)
      CALL GTSRCP_i('MXLV0_IFIELD',IFIELD,1)
      CALL GTSRCP('MXLV0_FIELDM',FIELDM,1)
      CALL GTSRCP('MXLV0_TMAXFD',TMAXFD,1)
      CALL GTSRCP('MXLV0_DMXMS',DMXMS,1)
      CALL GTSRCP('MXLV0_DEEMAX',DEEMAX,1)
      CALL GTSRCP('MXLV0_EPSIL',EPSIL,1)
      CALL GTSRCP('MXLV0_STMIN',STMIN,1)
C
C !The format of this array is as follows.
C ! 1st word = Number of mixtures. (NMIXT)
C ! 2nd,3,4 words = Hollerith string naming the mixture (terminated by a $ sign)
C ! 5th word = Mixture number used by Geant
C ! 6th word = Number of materials in the mixtures (NLMAT)
C ! if NLMAT is negative, DL contains WMAT molecular proportions.
C ! See GEANT MANUAL
C ! This is followed by NLMAT material numbers and NLMAT thicknesses (DL)
C ! this is followed by the density if NLMAT is negative
C ! This whole pattern is repeated NMIXT times
C
C  DEFINE MATERIALS
C
      NMIX = LMIX(1)
      IPT = 2
C
      DO 10 J=1,NMIX
        DENS=0.
        TOTL=0.
        TOTABS=0.
        TOTRAD=0.
        CALL UCOPY_i(LMIX(IPT),MIXNA1,3)
        CALL UHTOC( MIXNA1,3,MIXNAC,12)
        IMXNO = LMIX(IPT+3)
        NLMAT = LMIX(IPT+4)
        NLMATA = IABS(NLMAT)
        CALL UCOPY_i(LMIX(IPT+5),IMXMAT(1),NLMATA)
        CALL UCOPY(RMIX(IPT+5+NLMATA),DL(1),NLMATA)
        IPT = IPT + 5 + 2*NLMATA
        IF ( NLMAT.LT.0 ) THEN
          DENS = RMIX(IPT)
          IPT = IPT+1
        ENDIF
        DO 5 I=1,NLMATA
          CALL GFMATE(IMXMAT(I),DUM,AA(I),ZZ(I),D,RL,AL,
     +                UB,NU)
          IF ( NLMAT.GT.0 ) THEN
            DENS=DENS+D*DL(I)
            TOTL=TOTL+DL(I)             !Total length per cell
            TOTABS=TOTABS+DL(I)/AL      !Total absorption length per cell.
            TOTRAD=TOTRAD+DL(I)/RL      !Total radiation length per cell.
            WMAT(I)=D*DL(I)
          ELSE
            WMAT(I) = DL(I)
          ENDIF
    5   CONTINUE
        AEFF=0.
        ZEFF=0.
        DO 6 I=1,NLMATA
          AEFF=AEFF+AA(I)*WMAT(I)
          ZEFF=ZEFF+ZZ(I)*WMAT(I)         !Effective A and Z
          IF ( NLMAT.GT.0 ) THEN
            WMAT(I)=WMAT(I)/DENS
          ENDIF
    6   CONTINUE
        IF(NLMAT.GT.0)THEN
          DENS=DENS/TOTL
          TOTABS=TOTL/TOTABS              !Absorption length of mixture
          TOTRAD=TOTL/TOTRAD              !Radiation length of mixture
        ENDIF
        IF(DLAM.NE.0)THEN
          CALL GSMATE(IMXNO,MIXNAC,AEFF,ZEFF,DENS,TOTRAD,TOTABS,0,0)
        ELSE
          CALL GSMIXT(IMXNO,MIXNAC,AA,ZZ,DENS,NLMAT,WMAT)
        ENDIF
C
C  DEFINE MEDIA
C

        CALL GSTMED(IMXNO,MIXNAC,IMXNO,ISVOL,IFIELD,FIELDM,
     &    TMAXFD,DMXMS,DEEMAX,EPSIL,STMIN, 0,0)
C
   10 CONTINUE
      END
