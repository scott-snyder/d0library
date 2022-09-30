      SUBROUTINE MXCAL(NMSRCP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Defines materials for CC/EC calorimeters
C-                         Calls GSMIXT if DLAM = 0
C-                         else CALLS GSMATE with Absorption
C-                         Lengths we calculate
C-
C-   Inputs  : NMSRCP = SRCP ident
C-   Outputs : None
C-   Controls: None
C-
C-   Created  01-DEC-1985   A.M.Jonckheere,Rajendran Raja
C-   Updated  19-OCT-1988   Rajendran Raja  .Does Molecular compounds also
C-   Updated  28-NOV-1988   Elliott A. Treadwell, Harrison B. Prosper
C-                          Change order in which GTSRCP calls are made.
C-                          Do call to GTSRCP (NMSRC1 ... FIRST then
C-                          select bank SRCP_REST and do other GTSRCP
C-                          calls. Note, however, that the selection of
C-                          SRCP_REST is temporary. The bank which was
C-                          selected before entering the routine will be
C-                          automatically reselected.
C-   Updated  30-MAR-1990   S. Linn    Upgrade to GEANT 3.13:
C-                          Use charcter variable in calls to
C-                          GSMATE,GSMIXT,GSTMED; add calls to GSTPAR
C-   Updated  21-FEB-1991   K. Wyatt Merritt   Merged in changes by
C-                          Natalie Roe and Stu Fuess to test beam version
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) NMSRCP
      CHARACTER*32 NMSRC1
      INTEGER LEN3
C
      INCLUDE 'D0$INC:D0LOG.INC'
      INCLUDE 'D0$INC:DCALOG.INC'
C
      INTEGER NLMAT,NMIX,IMXNO,J,IPT,NLMATA
C
      REAL AA(10),ZZ(10),DENS,WMAT(10),TOTL,D,RL,AL
      REAL UB,TOTABS,TOTRAD,AEFF,ZEFF,WTOT
C
      CHARACTER*12 MIXNAC
      INTEGER MIXNA1(3)
C  DEFINE MIXTURE DATA, NMIX TYPES
      INTEGER IMXMAT(10),I,NU
      REAL DL(10)
      CHARACTER*20 DUM
C
      INTEGER MAXMIX
      PARAMETER (MAXMIX = 1000)
      INTEGER LMIX(MAXMIX)
      REAL RMIX(MAXMIX)
      EQUIVALENCE ( LMIX,RMIX )
C
      INTEGER ISVOL,IFIELD
      REAL FIELDM,TMAXFD,DMXMS,DEEMAX,EPSIL,STMIN

C
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
      CALL SLSRCP('SRCP_REST') ! Select general SRCP bank
      CALL GTSRCP_i('MXCAL_ISVOL',ISVOL,1)
      CALL GTSRCP_i('MXCAL_IFIELD',IFIELD,1)
      CALL GTSRCP('MXCAL_FIELDM',FIELDM,1)
      CALL GTSRCP('MXCAL_TMAXFD',TMAXFD,1)
      CALL GTSRCP('MXCAL_DMXMS',DMXMS,1)
      CALL GTSRCP('MXCAL_DEEMAX',DEEMAX,1)
      CALL GTSRCP('MXCAL_EPSIL',EPSIL,1)
      CALL GTSRCP('MXCAL_STMIN',STMIN,1)
      CALL RSSRCP ! Reset to original SRCP bank
C
C !The format of this array is as follows.
C ! 1st word = Number of mixtures. (NMIXT)
C ! 2nd,3,4 words = Hollerith string naming the mixture (terminated by a $ sign)
C ! 5th word = Mixture number used by Geant
C ! 6th word = Number of materials in the mixtures (NLMAT)
C ! if NLMAT is negative, DL contains WMAT molecular proportions.
C ! See GEANT MANUAL
C ! If |NLMAT|>1 then:
C ! This is followed by NLMAT material numbers and NLMAT thicknesses (DL)
C ! this is followed by the density if NLMAT is negative
C ! If |NLMAT|=1 then
C ! This is followed by Aeff, Zeff, density, rad. length, abs. length
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
        WTOT=0.
        CALL UCOPY_i(LMIX(IPT),MIXNA1,3)
        CALL UHTOC(MIXNA1,4,MIXNAC,12)
        IMXNO = LMIX(IPT+3)
        NLMAT = LMIX(IPT+4)
        NLMATA = IABS(NLMAT)
        IF (NLMATA.GE.2) THEN
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
              WMAT(I) = DL(I)*AA(I)
              WTOT=WTOT+DL(I)*AA(I)
            ENDIF
    5     CONTINUE
          AEFF=0.
          ZEFF=0.
          DO 6 I=1,NLMATA
            IF ( NLMAT.GT.0 ) THEN
              WMAT(I)=WMAT(I)/DENS
            ELSE
              WMAT(I)=WMAT(I)/WTOT
            ENDIF
            AEFF=AEFF+AA(I)*WMAT(I)
            ZEFF=ZEFF+ZZ(I)*WMAT(I)         !Effective A and Z
    6     CONTINUE
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
        ELSE                ! NLMAT=1:  Material, not mixture
          AEFF=RMIX(IPT+5)
          ZEFF=RMIX(IPT+6)
          DENS=RMIX(IPT+7)
          TOTRAD=RMIX(IPT+8)
          TOTABS=RMIX(IPT+9)
          IPT=IPT+10
          CALL GSMATE(IMXNO,MIXNAC,AEFF,ZEFF,DENS,TOTRAD,TOTABS,0,0)
        ENDIF
C
C  DEFINE MEDIA
C
        CALL GSTMED(IMXNO,MIXNAC,IMXNO,ISVOL,IFIELD,FIELDM,
     &    TMAXFD,DMXMS,DEEMAX,EPSIL,STMIN, 0,0)
C
C  Steve Linn's version.
C
      IF( SHWG.EQ.7)THEN
        CALL GSTPAR( IMXNO,'CUTGAM',LOECUT)
        CALL GSTPAR( IMXNO,'CUTELE',LOECUT)
        CALL GSTPAR( IMXNO,'CUTHAD',.01 )
        CALL GSTPAR( IMXNO,'CUTNEU',.01 )
        CALL GSTPAR( IMXNO,'MULS', 0.)
        CALL GSTPAR( IMXNO,'PFIS', 0.)
        CALL GSTPAR( IMXNO,'MUNU', 0.)
        CALL GSTPAR( IMXNO,'DRAY', 0.)
        CALL GSTPAR( IMXNO,'PAIR', 1.)
        CALL GSTPAR( IMXNO,'BREM', 1.)
        CALL GSTPAR( IMXNO,'HADR', 1.)
        CALL GSTPAR( IMXNO,'DCAY', 1.)
        CALL GSTPAR( IMXNO,'LOSS', 1.)
        CALL GSTPAR( IMXNO,'COMP', 2.)
        CALL GSTPAR( IMXNO,'PHOT', 2.)
        CALL GSTPAR( IMXNO,'ANNI', 2.)
      ENDIF
   10 CONTINUE
      END
