C----------------------------------------------------------------------
C-
C-   Created  26-APR-1991   Alan M. Jonckheere - Geant 3.14
C-
C----------------------------------------------------------------------
      INTEGER MSLIST,NPHI,IPHIMI,IPHIMA,IPHI1,IPHIL,NTETA,MODTET,NSLMAX,
     +        MAXMDT,NSLIST,ISLIST,IPHI,ITETA,ISCUR
      REAL    PHIMIN,PHIMAX,DPHI,TETMIN,TETMAX,VSCAN,FACTX0,FACTL,
     +        FACTR,SX0,SABS,TETMID,TETMAD
C
      PARAMETER (MSLIST=32,MAXMDT=3)
      COMMON/GCSCAN/SCANFL,NPHI,PHIMIN,PHIMAX,NTETA,TETMIN,TETMAX,
     +              MODTET,IPHIMI,IPHIMA,IPHI1,IPHIL,NSLMAX,
     +              NSLIST,ISLIST(MSLIST),VSCAN(3),FACTX0,FACTL,
     +              FACTR,IPHI,ITETA,ISCUR,SX0,SABS,TETMID(MAXMDT),
     +              TETMAD(MAXMDT)
      LOGICAL SCANFL
      COMMON/GCSCAC/SFIN,SFOUT
      CHARACTER*80 SFIN,SFOUT
C
