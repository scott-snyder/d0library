C----------------------------------------------------------------------
C-
C-   Created  24-FEB-1989   A. Zylberstejn
C-
C----------------------------------------------------------------------
      INTEGER NTOTTR
      PARAMETER( NTOTTR =100 )
      COMMON /TRINTR/ TRUNUM(NTOTTR),ANOINF(3,NTOTTR),ANOSUP(3,NTOTTR),
     &  CATINF(3,NTOTTR),CATSUP(3,NTOTTR),NANOD(3,NTOTTR),
     &  NCATO(3,NTOTTR),ENERGP(NTOTTR),STHETA(NTOTTR),PHIP(NTOTTR),
     &  IDENTR(NTOTTR),NGOODT
      INTEGER IDENTR
      INTEGER TRUNUM,ANOINF,ANOSUP,CATINF,CATSUP,NCATO,NANOD,NGOODT
      REAL              ENERGP        ,STHETA        ,PHIP 
