C----------------------------------------------------------------------
C-
C-   Temporarily stores various FDC event info for later use.
C-
C-   Created  27-AUG-1990   Jeffrey Bantly
C-
C----------------------------------------------------------------------
      COMMON /FDSPEC/ EARLY,EFOUND(2000),ICNT,FIRSTBIN(2000),
     &  SECONDBIN(2000),THIRDBIN(2000),HIGHBIN(2000),TRKTOT,
     &  NTRKHIT(2000),TRKRESID(78000),XTRACK(2000),IXTRACK,
     &  XPVAR(200),XTVAR(200),PVAR(200),TVAR(200),IXPSG,IXTSG,IPSG,ITSG,
     &  GOODTRACK(2000),onez(0:1),tothit,tothits
      INTEGER FIRSTBIN,ICNT,SECONDBIN,THIRDBIN,HIGHBIN,XTRACK,NTRKHIT,
     &  TRKTOT,IXTRACK,IXPSG,IXTSG,IPSG,ITSG,onez,tothit,tothits
      REAL    TRKRESID,XPVAR,XTVAR,PVAR,TVAR
      LOGICAL EARLY,EFOUND,GOODTRACK
