C----------------------------------------------------------------------
C-
C-   Created   15-FEB-1993  Andrew G. Brandt
C-   Updated   27-SEP-1993  Andrew G. Brandt added new GLOB words
C-   Updated   01-NOV-1994  Andrew G. Brandt updated for CW 
C----------------------------------------------------------------------
C
C QCD_GLOB1
C
      COMMON /QCD_GLOB1/ETOT,TIME29,MRBITS
C
      REAL    ETOT,TIME29
      INTEGER MRBITS
      CHARACTER*255 QCD_GLOB1_TAG
      DATA QCD_GLOB1_TAG/'E_TOT:R,T29:R,MRBITS:I'/
C
C QCD_GLOB2
C
      COMMON /QCD_GLOB2/NCDCTK,NCELCC,ETGLOB(3),EGLOB(8)
C
      REAL    ETGLOB,EGLOB
      INTEGER NCDCTK,NCELCC
      CHARACTER*255 QCD_GLOB2_TAG
      DATA QCD_GLOB2_TAG/
     &  'NCDC[-1,4094]:I,NCEL[-1,4094]:I,ET_TOT:R,ET_EC:R,
     &   ET_CC:R,E_ECS:R,E_CCUP:R,E_CCDN:R,E_ECN:R,
     &   E_MR:R,ET_MR:R,E_HOT:R,VET_HOT:R'/
