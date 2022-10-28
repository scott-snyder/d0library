      SUBROUTINE FSUB1(M,U1,F1,X)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Does the integral over X1 to get the
C-    cross section
C-
C-   Inputs  : M= INTEGER SET BY CALLING FUNCTION DGMLT1
C-   U1 = ONE DIMENSIONAL ARRAY WITH DIMENSION M WITH CONTENTS SET BY
C-   DGMLT1
C-   Outputs : F1 = One dimensional array of dimension
C-   M whose contents are set by
C-   this function  are the values of the integrand at the values
C-   of X provided in array U1
C-   See Cernlib D110 for more info

C-   Controls:
C-
C-   Created  29-MAR-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PARTON_KINE.INC'
      INCLUDE 'D0$INC:TOP_SOLNSE.INC'
      INTEGER M
      DOUBLE PRECISION U1(M),F1(M),X(*)
      INTEGER L,NG2,NI2,IER
      INTEGER STR_FN_NPTYPE,STR_FN_NGROUP,STR_FN_NSET
      INTEGER STR_FN_NFL,STR_FN_LO
C
      CHARACTER*20 PARM(20)
      DOUBLE PRECISION VAL(20)
C
      REAL    STR_FN_SCALE
      DOUBLE PRECISION DSCALE,DUPV,DDNV,DSEA,DSTR,DCHM,DBOT,DTOP,DGL
      DOUBLE PRECISION DXA,DXB,FA,FB
      DOUBLE PRECISION ALPHAS2
      REAL    DSIGMA_DT_QUARK,DSIGMA_DT_GLUON
      LOGICAL first
      SAVE first
      DATA first / .true. /
C
      REAL    T
C----------------------------------------------------------------------
      IF( first ) THEN
        first = .false.
        CALL EZPICK('TOP_MASS_RCP')
C
        CALL EZGET_i('STRUCTURE_FN_NPTYPE',STR_FN_NPTYPE,IER)
        CALL EZGET_i('STRUCTURE_FN_NGROUP',STR_FN_NGROUP,IER)
        CALL EZGET_i('STRUCTURE_FN_NSET',STR_FN_NSET,IER)
C
        CALL EZGET_i('STRUCTURE_FN_NFL',STR_FN_NFL,IER)
        CALL EZGET_i('STRUCTURE_FN_LO',STR_FN_LO,IER)
        CALL EZGET('STRUCTURE_FN_SCALE',STR_FN_SCALE,IER)
        CALL EZRSET
C
        PARM(1) = 'NPTYPE'
        VAL(1) = STR_FN_NPTYPE
        PARM(2) = 'NGROUP'
        VAL(2) = STR_FN_NGROUP
        PARM(3) = 'NSET'
        VAL(3) = STR_FN_NSET
        PARM(4) = 'NFL'
        VAL(4) = STR_FN_NFL
        PARM(5) = 'LO'
        VAL(5) = STR_FN_LO
C
        CALL PDFSET(PARM,VAL)
C
      ENDIF
C
      DSCALE = STR_FN_SCALE*TMASSE  !SCALE OF STRUCTURE FUNCTIONS
      ALPHA_S = ALPHAS2(DSCALE)
C
      TMASS = TMASSE   !To communicate to dsigma_dt routines
C
      DXB = X(2)
      DXA = X(3)
C
C ****  COMPUTE STRUCTURE FUNCTION PRODUCT  HERE BASED ON QUARK OR GLUON
C ****  CALL APPROPRIATE T DISTRIBUTION
C
      CALL STRUCTF(DXA,DSCALE,DUPV,DDNV,DSEA,DSTR,DCHM,DBOT,DTOP,DGL)
      IF ( QUARK ) THEN
        FA = (DUPV+DDNV)/DXA
      ELSE
        FA = DGL/DXA
      ENDIF
C
      CALL STRUCTF(DXB,DSCALE,DUPV,DDNV,DSEA,DSTR,DCHM,DBOT,DTOP,DGL)
      IF ( QUARK ) THEN
        FB = (DUPV+DDNV)/DXB
      ELSE
        FB = DGL/DXB
      ENDIF
C
      SH = SHATD  !FOR DSIGMA ROUTINE
C
      DO 10 L = 1 , M
        X(1) = U1(L)  !T DISTRIBUTION
        T = X(1)
        IF ( QUARK ) THEN
          F1(L) = FA*FB*DSIGMA_DT_QUARK(T)
        ELSE
          F1(L) = FA*FB*DSIGMA_DT_GLUON(T)
        ENDIF
   10 CONTINUE
C
  999 RETURN
      END
