C------------------------------------------------------------------------
      LOGICAL FUNCTION ISOL_MU_SELECT
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Apply full isolated muon selection to muons
C-    selected in PARTICLE_SELECT...
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  25-Apr-1994   John D. Hobbs
C-   Updated  11-DEC-1994   Meenakshi Narain   add cut on jet_et for isolation
C-                              fix the output value for DR
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INTEGER NMAX
      PARAMETER(NMAX=10)
      INTEGER NMUONS,LMUONS(NMAX),NJETS,LJETS(NMAX)
      INTEGER LPMUO,LJET,LCLOSE,I,J,IERR,IER,LTEMP
      REAL    DRCLOSE,DETA,DPHI,DRR,DEDIFF,DELTAE,DELTAR,JET_ET
      CHARACTER*(*) ERRNAME
      PARAMETER(ERRNAME='ISOL_MU_SELECT')
C
      INTEGER NISOL,LINK_NUM(NMAX),QUAD
      REAL    DR(NMAX),EDIFF(NMAX)
      SAVE    NISOL,LINK_NUM,DR,EDIFF
C
      REAL    ISOL_MU_CF_MAX,ISOL_MU_EF_MAX,ISOL_MU_JET_ET_MIN
      REAL    ISOL_MU_DR_MIN
      SAVE    ISOL_MU_CF_MAX,ISOL_MU_EF_MAX,ISOL_MU_JET_ET_MIN
      SAVE    ISOL_MU_DR_MIN
C
      LOGICAL FIRST,ISOL_MU_GETPAR,ISOL_MU_SELECT_FIN
      DATA FIRST/.TRUE./
      SAVE FIRST
C-----------------------------------------------------------------------
C
      IF(FIRST) THEN
        FIRST=.FALSE.
        CALL INRCP('MUJETS_MU_SELECT_RCP',IERR)
        IF( IERR.NE.0 ) CALL ERRMSG('MUJETS_MU_SELECT_NORCP',ERRNAME,
     >      'RCP not found','F')
        CALL EZGET('EDIFF_CF_MAX',ISOL_MU_CF_MAX,IERR)
        IF(IERR.EQ.0) CALL EZGET('EDIFF_EF_MAX',ISOL_MU_EF_MAX,IERR)
        IF(IERR.EQ.0) CALL EZGET('JET_ET_MIN',ISOL_MU_JET_ET_MIN,IERR)
        IF(IERR.EQ.0) CALL EZGET('DR_MIN',ISOL_MU_DR_MIN,IERR)
        IF(IERR.NE.0) CALL ERRMSG('ISOL_MU_SELECT_RCPERR',
     >      ERRNAME,'RCP Parameter Error','F')
      ENDIF
      ISOL_MU_SELECT=.TRUE.
      NISOL=0
C
      CALL GTSLINK('HARD_MUO',NMAX,NMUONS,LMUONS)
      CALL GTSLINK('TOP_JETS',NMAX,NJETS,LJETS)
C
      DO 20 I=1,NMUONS
C    Check muon only quantities
        LPMUO=LMUONS(I)
        DELTAE=Q(LPMUO+35)-Q(LPMUO+34)
        QUAD=IQ(LPMUO+7)
        IF(QUAD.LT.5.AND.DELTAE.GT.ISOL_MU_CF_MAX)               GOTO 20
        IF(QUAD.GE.5.AND.QUAD.LT.13.AND.DELTAE.GT.ISOL_MU_EF_MAX)GOTO 20
        IF(QUAD.GT.12)                                           GOTO 20
C    Check jet/muon variables
        DRCLOSE=999.9
        DO 10 J=1,NJETS
          LJET=LJETS(J)
          DETA=Q(LJET+9)-Q(LPMUO+16)
          DPHI=ABS(Q(LJET+8)-Q(LPMUO+17))
          IF( DPHI.GT.3.1416 ) DPHI=6.2832-DPHI
          DELTAR=SQRT(DETA**2+DPHI**2)
          IF( DELTAR.LT.DRCLOSE ) THEN
            DRCLOSE=DELTAR
            JET_ET = Q(LJET+6)
            LCLOSE=LJET
          ENDIF
 10     CONTINUE
C
C  Compute extra quantities and apply muon/jet cuts.
C
        IF( DRCLOSE.LT.ISOL_MU_DR_MIN .AND. DRCLOSE.NE.999.9 ) THEN
          IF (JET_ET.GT.ISOL_MU_JET_ET_MIN) GOTO 20
        ENDIF
C
C  This is an isolated muon.  Save it in protected area
C
        IF( NISOL.LT.NMAX ) THEN
          NISOL=NISOL+1
          DR(NISOL)=DRCLOSE
          EDIFF(NISOL)=DELTAE
          CALL GSLINK('ISOLMUON',LINK_NUM(NISOL))
          LSLINK(LINK_NUM(NISOL))=LPMUO
        ELSE
          CALL ERRMSG(' ','ISOL_MU_SELECT','Link count exceeded','W')
          ISOL_MU_SELECT=.FALSE.
        ENDIF
 20   CONTINUE
      GOTO 999
C-----------------------------------------------------------------------
      ENTRY ISOL_MU_GETPAR(LTEMP,DRR,DEDIFF)
C-----------------------------------------------------------------------
      ISOL_MU_GETPAR=.FALSE.
      IER=1
      DO 30 I=1,NISOL
        IF( LTEMP.NE.LSLINK(LINK_NUM(I)) ) GOTO 30
        ISOL_MU_GETPAR=.TRUE.
        DRR=DR(I)
        DEDIFF=EDIFF(I)
        GOTO 999
 30   CONTINUE
      GOTO 999
C-----------------------------------------------------------------------
      ENTRY ISOL_MU_SELECT_FIN
C----------------------------------------------------------------------
      ISOL_MU_SELECT_FIN=.TRUE.
      DO I=1,NISOL
        CALL RSLINK('ISOLMUON',LINK_NUM(I))
      ENDDO
      GOTO 999
 999  CONTINUE
      RETURN
      END
 
