C     FDCPRM.INC
C----------------------------------------------------------------------
C-   Purpose: Permanent link area for the FDC STP banks
C-
C-   Created  18-DEC-1989   Jeffrey Bantly
C----------------------------------------------------------------------
      COMMON /FDCPRM/ FDCPRM,LFGHF,LFGUN,LFGQD,LFGSE,
     +    LFMAT,LFWAL,LFWTA,LFWTB,LFWPH,LFDRT,LFDTA,LFDTB,LFDPH,
     +    LFALH,LFAHF,LFAUN,LFAQD,LFASE,
     +    LFTHF,LFTUN,LFTQU,LFTSE,LFPHF,LFPUN,LFPQD,LFPSE
      INTEGER FDCPRM(2),LFGHF(0:1),LFGUN(0:1,0:1),
     +    LFGQD(0:1,0:7),LFGSE(0:1,0:1,0:7,0:35)
      INTEGER LFMAT,LFWAL,LFWTA,LFWTB,LFWPH,
     +    LFDRT,LFDTA,LFDTB,LFDPH
      INTEGER LFALH,LFAHF(0:1),LFAUN(0:1,0:1),
     +    LFAQD(0:1,0:7),LFASE(0:1,0:1,0:7,0:35)
      INTEGER LFTHF(0:1),LFTUN(0:1,0:1),
     +    LFTQU(0:1,0:7),LFTSE(0:1,0:1,0:7,0:35)
      INTEGER LFPHF(0:1),LFPUN(0:1,0:1),
     +    LFPQD(0:1,0:7),LFPSE(0:1,0:1,0:7,0:35)
C----------------------------------------------------------------------
