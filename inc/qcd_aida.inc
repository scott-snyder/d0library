C----------------------------------------------------------------------
C-
C-   Created   06-DEC-1995  Andrew G. Brandt
C-   Updated   11-MAR-1996  Andrew G. Brandt add ILYR
C-
C----------------------------------------------------------------------
C
C QCD_AIDA
C
      INTEGER NAIDMX
      PARAMETER (NAIDMX=20)
      COMMON /QCD_AIDA/ NAIDA, AIDET(NAIDMX), AIDETA(NAIDMX), 
     +        AIDPHI(NAIDMX), AIDLYR(NAIDMX)
C
      REAL    AIDET, AIDETA, AIDPHI
      INTEGER NAIDA, AIDLYR
      CHARACTER*255 QCD_AIDA_TAG
      DATA QCD_AIDA_TAG/'NAID[0,20],AID_ET(NAID):R,AID_ETA(NAID):R,
     +                   AID_PHI(NAID):R,AID_LYR(NAID)[-1,30]:I'/
