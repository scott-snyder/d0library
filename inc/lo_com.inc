C----------------------------------------------------------------------
C-
C-   Created  18-Oct-1993   Sandor Feher and Patrick Mooney
C-
C----------------------------------------------------------------------
C
C   specifying PDFLIB and file parameters
C
      INTEGER     LUN,IER,LEN
      CHARACTER*80 FILENAME
C
C   specifying variables for choice of Pt scale and TRIPLE DIFFERENTIAL X-SEC
C
      INTEGER     LIMIT_FLAG,TRIPLE_DIFF_FLAG
      REAL*8      PT_SCALE_FACTOR
C
C   variables related to bins for which the differential cross section is
C    calculated
C
      REAL*8      VARLOW1,VARUP1,SVAR1,VARLOW2,VARUP2,SVAR2
      REAL*8      VARLOW3,VARUP3,SVAR3
C
C   specifying KINEMATIC, DYNAMIC and CALCULATED variables
C
      REAL*8      VAR1,VAR2,VAR3
      REAL*8      ETA1,ETA2,PT,ETA_STR,MJJ,ETA_BST,XA,XB
      REAL*8      SQ_S,DSIG_DT(12),D3S_E1_E2_PT2
C
C   specify COMMON BLOCK
C
      COMMON /LO_COM_1/ LUN,IER,LEN,
     &                  LIMIT_FLAG,TRIPLE_DIFF_FLAG 
      COMMON /LO_COM_2/ PT_SCALE_FACTOR,
     &                  VARLOW1,VARUP1,SVAR1,VARLOW2,VARUP2,SVAR2,
     &                  VARLOW3,VARUP3,SVAR3,VAR1,VAR2,VAR3,
     &                  ETA1,ETA2,PT,ETA_STR,MJJ,ETA_BST,XA,XB,
     &                  SQ_S,DSIG_DT,D3S_E1_E2_PT2
      COMMON /LO_COM_3/ FILENAME
C----------------------------------------------------------------------
