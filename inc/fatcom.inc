C----------------------------------------------------------------------
C-
C-   Created   7-OCT-1991   Krzysztof L. Genser
C-   Updated   5-NOV-1991   Krzysztof L. Genser
C-   Updated  19-MAY-1994   Krzysztof L. Genser  FATLNK,FATCOM->D0FMLK,D0FMCM
C-   Updated  10-OCT-1995   Stan M. Krzywdzinski Increased LFMLAS=500000
C-
C----------------------------------------------------------------------

      INTEGER LFMR,LFMW,LFM2,LFMLAS
      PARAMETER (LFMR=1,LFMW=1,LFM2=5000,LFMLAS=500000)
      INTEGER LFMFNC
      PARAMETER( LFMFNC = 10 )
      INTEGER NFMDII
      PARAMETER( NFMDII = 50000  )
      INTEGER MFMLNK
      PARAMETER (MFMLNK=1000)

      INTEGER IFMSTR,IFMDIV,IFMFNC(LFMFNC),LFMEV,LFMEIN
      REAL    RFMVEC(LFMLAS)
      COMMON /D0FMCM/ IFMSTR,IFMDIV,IFMFNC,LFMEV,LFMEIN,RFMVEC

      INTEGER IFMQA
      PARAMETER( IFMQA = 999 )
      INTEGER LFMQ,IFMQ
      REAL    RFMQ
      DIMENSION LFMQ(IFMQA),IFMQ(IFMQA),RFMQ(IFMQA)
      EQUIVALENCE (IFMQ(1),RFMQ(1),LFMQ(9))
      EQUIVALENCE                 (LFMQ(1),LFMEV)
C
C ****  LFM%%F are reserved keywords for protected i/o file banks
C
      INTEGER        LFMUS1,LFMINF,LFMOUF,LFMUSK(MFMLNK),LFMUSL
      COMMON /D0FMLK/LFMUS1,LFMINF,LFMOUF,LFMUSK,LFMUSL
C
C ****  the common D0 prefix
C
      CHARACTER*(*) D0TRNK
C
C ****  Herb's unix patch
C
C&IF VAXVMS
C&      PARAMETER( D0TRNK = '//FNAL/D0' )
C&ELSE
      PARAMETER( D0TRNK = '//XXXX/XX' )
C&ENDIF
