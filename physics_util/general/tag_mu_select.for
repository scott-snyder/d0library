C------------------------------------------------------------------------
      LOGICAL FUNCTION TAG_MU_SELECT
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods:
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  25-Apr-1994   John D. Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INTEGER NMAX
      PARAMETER(NMAX=10)
      INTEGER NMUONS,LMUONS(NMAX),NJETS,LJETS(NMAX)
      INTEGER LPMUO,LJET,LCLOSE,J,I,IERR,IER,LTEMP
      REAL    DELTAR,DELTAP,DRCLOSE,DETA,DPHI,DRR,DPT,PTOTMU,PTOTJ
      CHARACTER*(*) ERRNAME
      PARAMETER(ERRNAME='TAG_MU_SELECT')
C
      INTEGER  NBTAG,LINK_NUM(NMAX)
      REAL     VDOTN
      LOGICAL  GOOD_PARTICLE
      EXTERNAL GOOD_PARTICLE,VDOTN
C
      LOGICAL REJECT_ISOL
      REAL    DR(NMAX),PTREL(NMAX)
      REAL    DR_MAX,PTREL_MIN,PTREL_MAX
      SAVE    REJECT_ISOL,DR_MAX,PTREL_MIN,PTREL_MAX,DR,PTREL
      SAVE    NBTAG,LINK_NUM
C
      LOGICAL FIRST,TAG_MU_GETPAR,TAG_MU_SELECT_FIN
      DATA FIRST/.TRUE./
      SAVE FIRST
C-----------------------------------------------------------------------
C
      IF(FIRST) THEN
        FIRST=.FALSE.
        CALL INRCP('MUJETS_MU_SELECT_RCP',IERR)
        IF( IERR.NE.0 ) CALL ERRMSG('MUJETS_MU_SELECT_NORCP',ERRNAME,
     >      'RCP not found','F')
        CALL EZGET('DR_MAX',DR_MAX,IERR)
        IF(IERR.EQ.0) CALL EZGET('PTREL_MIN',PTREL_MIN,IERR)
        IF(IERR.EQ.0) CALL EZGET('PTREL_MAX',PTREL_MAX,IERR)
        IF(IERR.EQ.0) CALL EZGET('REJECT_ISOL',REJECT_ISOL,IERR)
        IF(IERR.NE.0) CALL ERRMSG('TAG_MU_SELECT_RCPERR',ERRNAME,
     >      'RCP Parameter Error','F')
      ENDIF
      TAG_MU_SELECT=.TRUE.
      NBTAG=0
C
      CALL GTSLINK('SOFT_MUO',NMAX,NMUONS,LMUONS)
      CALL GTSLINK('TOP_JETS',NMAX,NJETS,LJETS)
C
      DO 20 I=1,NMUONS
C    Check muon only quantities
        LPMUO=LMUONS(I)
        IF( GOOD_PARTICLE(LPMUO,'ISOLMUON') .AND. REJECT_ISOL ) GOTO  20
C    Find closest Jet...
        DRCLOSE=999.9
        DO 10 J=1,NJETS
          LJET=LJETS(J)
          DETA=Q(LJET+9)-Q(LPMUO+16)
          DPHI=ABS(Q(LJET+8)-Q(LPMUO+17))
          IF( DPHI.GT.3.1416 ) DPHI=6.2832-DPHI
          DELTAR=SQRT(DETA**2+DPHI**2)
          IF( DELTAR.LT.DRCLOSE ) THEN
            DRCLOSE=DELTAR
            LCLOSE=LJET
          ENDIF
 10     CONTINUE
C
C  Check jet/muon variables...
C
        DELTAR=DRCLOSE
        DELTAP=-1.0
        IF( NJETS.GT.0 ) THEN
          LJET=LCLOSE
          PTOTMU=SQRT(Q(LPMUO+10)**2+Q(LPMUO+11)**2+Q(LPMUO+12)**2)
          PTOTJ=SQRT(Q(LJET+2)**2+Q(LJET+3)**2+Q(LJET+4)**2)
          IF( PTOTMU.NE.0 .AND. PTOTJ.NE.0 ) THEN
            DELTAP=SQRT(1.0-VDOTN(Q(LPMUO+10),Q(LJET+2),3)**2)
            DELTAP=Q(LPMUO+13)*DELTAP
            IF( DELTAP.LT.PTREL_MIN ) GOTO 20
            IF( DELTAP.GT.PTREL_MAX ) GOTO 20
          ENDIF
          IF( DELTAR.GT.DR_MAX ) GOTO 20
        ENDIF
C
C  This is a tag muon.  Save it in protected area...
C
        IF( NBTAG.LT.NMAX ) THEN
          NBTAG=NBTAG+1
          DR(NBTAG)=DELTAR
          PTREL(NBTAG)=DELTAP
          CALL GSLINK('BTAGMUON',LINK_NUM(NBTAG))
          LSLINK(LINK_NUM(NBTAG))=LPMUO
        ELSE
          CALL ERRMSG(' ','TAG_MU_SELECT','Link count exceeded','W')
        ENDIF
C
 20   CONTINUE
      GOTO 999
C-----------------------------------------------------------------------
      ENTRY TAG_MU_GETPAR(LTEMP,DRR,DPT)
C-----------------------------------------------------------------------
      TAG_MU_GETPAR=.FALSE.
      DO 30 I=1,NBTAG
        IF( LTEMP.NE.LSLINK(LINK_NUM(I)) ) GOTO 30
        TAG_MU_GETPAR=.TRUE.
        DRR=DR(I)
        DPT=PTREL(I)
        GOTO 999
 30   CONTINUE
      GOTO 999
C-----------------------------------------------------------------------
      ENTRY TAG_MU_SELECT_FIN
C----------------------------------------------------------------------
      TAG_MU_SELECT_FIN=.TRUE.
      DO I=1,NBTAG
        CALL RSLINK('BTAGMUON',LINK_NUM(I))
      ENDDO
      GOTO 999
C----------------------------------------------------------------------
 999  CONTINUE
      RETURN
      END
