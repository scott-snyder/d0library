      SUBROUTINE LO_IN_R
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  To read data and set variables using LO_RCP
C-
C-   Inputs  : LO_RCP
C-   Outputs : None
C-   Controls: None
C-
C-   Created  18-Oct-1993   Sandor Feher and Patrick Mooney
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C
      INCLUDE 'D0$INC:LO_COM.INC'
C
C----------------------------------------------------------------------
C
C  define DOUBLE PRECISION variables for calling sequence to PFTOPDG
C
      INTEGER     NPDFSET
      INTEGER     NPTYPE,NGROUP,NSET,NFL,LO
      INTEGER     LENGTH,I
      CHARACTER   PARM(20)*20
      REAL*8      VAL(20)
      REAL*8      TMAS,QCDL4,QCDL5
      REAL*8      XMIN,XMAX,Q2MIN,Q2MAX
C
C----------------------------------------------------------------------
C
C
C ****  ZEBRA and HBOOK initialization
C
      CALL INZBRA
      CALL INRCP('LO_RCP',IER)
      IF ( IER.EQ.0 ) THEN
        CALL GTUNIT(77,LUN,IER)
        I = 1
        CALL EZGETS('NTUPLE_FILE',I,FILENAME,LEN,IER)
        CALL EZPICK ('LO_RCP')
        CALL EZGET_i('NPTYPE',NPTYPE,IER)
        CALL EZGET_i('NGROUP',NGROUP,IER)
        CALL EZGET_i('NSET',NSET,IER)
        CALL EZGET_i('NFL',NFL,IER)
        CALL EZGET_i('LO',LO,IER)
        CALL EZGET('TMAS',TMAS,IER)
        CALL EZGET('QCDL4',QCDL4,IER)
        CALL EZGET('QCDL5',QCDL5,IER)
        CALL EZGET('XMIN',XMIN,IER)
        CALL EZGET('XMAX',XMAX,IER)
        CALL EZGET('Q2MIN',Q2MIN,IER)
        CALL EZGET('Q2MAX',Q2MAX,IER)
        CALL EZGET_i('LIM_FLAG',LIMIT_FLAG,IER)
        CALL EZGET_i('FLAG',TRIPLE_DIFF_FLAG,IER)
        CALL EZGET('SQ_S',SQ_S,IER)
        CALL EZGET('PT_SCALE_FACTOR',PT_SCALE_FACTOR,IER)
        CALL EZGET('V1_LOW',VARLOW1,IER)
        CALL EZGET('V1_UP',VARUP1,IER)
        CALL EZGET('V1_STEP',SVAR1,IER)
        CALL EZGET('V2_LOW',VARLOW2,IER)
        CALL EZGET('V2_UP',VARUP2,IER)
        CALL EZGET('V2_STEP',SVAR2,IER)
        CALL EZGET('V3_LOW',VARLOW3,IER)
        CALL EZGET('V3_UP',VARUP3,IER)
        CALL EZGET('V3_STEP',SVAR3,IER)
        CALL EZRSET
      ELSE
        STOP 'Could not read LO_RCP'
      ENDIF
C
C   set PDFLIB parameters to choose a particular set of Parton Distributions
C
      NPDFSET = 0
      IF (NPTYPE.NE.0) THEN
        NPDFSET = NPDFSET + 1
        PARM(NPDFSET) = 'NPTYPE'
        VAL (NPDFSET) =  NPTYPE
      ENDIF
      IF (NGROUP.NE.0) THEN
        NPDFSET = NPDFSET + 1
        PARM(NPDFSET) = 'NGROUP'
        VAL (NPDFSET) =  NGROUP
      ENDIF
      IF (NSET.NE.0) THEN
        NPDFSET = NPDFSET + 1
        PARM(NPDFSET) = 'NSET'
        VAL (NPDFSET) =  NSET
      ENDIF
      IF(NPDFSET.NE.0) CALL PDFSET(PARM,VAL)
C
C   set other PDFLIB parameters
C
      NPDFSET = 0
      IF (NFL.NE.5) THEN
        NPDFSET = NPDFSET + 1
        PARM(NPDFSET) = 'NFL'
        VAL (NPDFSET) =  NFL
      ENDIF
      IF (LO.NE.2) THEN
        NPDFSET = NPDFSET + 1
        PARM(NPDFSET) = 'LO'
        VAL (NPDFSET) =  LO
      ENDIF
      IF (TMAS.NE.100.0) THEN
        NPDFSET = NPDFSET + 1
        PARM(NPDFSET) = 'TMAS'
        VAL (NPDFSET) =  TMAS
      ENDIF
      IF (QCDL4.NE.0.0) THEN
        NPDFSET = NPDFSET + 1
        PARM(NPDFSET) = 'QCDL4'
        VAL (NPDFSET) =  QCDL4
      ENDIF
      IF (QCDL5.NE.0.0) THEN
        NPDFSET = NPDFSET + 1
        PARM(NPDFSET) = 'QCDL5'
        VAL (NPDFSET) =  QCDL5
      ENDIF
      IF (XMIN.NE.0.0) THEN
        NPDFSET = NPDFSET + 1
        PARM(NPDFSET) = 'XMIN'
        VAL (NPDFSET) =  XMIN
      ENDIF
      IF (XMAX.NE.0.0) THEN
        NPDFSET = NPDFSET + 1
        PARM(NPDFSET) = 'XMAX'
        VAL (NPDFSET) =  XMAX
      ENDIF
      IF (Q2MIN.NE.0.0) THEN
        NPDFSET = NPDFSET + 1
        PARM(NPDFSET) = 'Q2MIN'
        VAL (NPDFSET) =  Q2MIN
      ENDIF
      IF (Q2MAX.NE.0.0) THEN
        NPDFSET = NPDFSET + 1
        PARM(NPDFSET) = 'Q2MAX'
        VAL (NPDFSET) =  Q2MAX
      ENDIF
      IF(NPDFSET.NE.0) CALL PDFSET(PARM,VAL)
C
C----------------------------------------------------------------------
  999 RETURN
      END
