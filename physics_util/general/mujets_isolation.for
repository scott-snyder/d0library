      LOGICAL FUNCTION MUJETS_ISOLATION(IMUON,ITYPE)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Apply isolation requirements specified in
C-    MUJETS_MU_SELECT.RCP.  Called by MUJETS_MU_SELECT.
C-
C-     Possible isolation types:
C-               0 - No isolation
C-               1 - dR only.
C-               2 - ediff cone
C-               3 - ptrel
C-
C-   Inputs  : LPMUO - pointer to PMUO bank.
C-             IMUON - PMUO bank index
C-             ITYPE - particle type being tested (see MUJETS_MU_SELECT.RCP)
C-   Outputs :
C-   Controls:
C-
C-   Created  31-Jul-1995   John D. Hobbs
C-
C-   mujets_isolation.for,v
C-   Revision 2.5  1996/03/14  22:53:18  hobbs
C-   Fix to MUJETS_MU_SELECT to avoid compilation problem on IBM/AIX
C-
C-   Revision 2.4.1.4  1996/01/31  16:06:40  hobbs
C-   Protect against crashes from corrupted JETS banks
C-
C-   Revision 2.4.1.3  1995/10/27  17:47:06  hobbs
C-   Change to pnew calculation.
C-
C-   Revision 2.4.1.2  1995/10/27  15:30:23  hobbs
C-   Miscellaneous changes/bugfixes to MUJETS_ISOLATION
C-
C-   Revision 2.4.1.1  1995/10/16  22:33:31  hobbs
C-   Bugfix to alternate vertex RCP flag
C-
C-   Revision 2.4  1995/10/13  22:17:19  hobbs
C-   Add alternate vertex calculation
C-
C-   Revision 2.3  1995/08/25  22:32:16  hobbs
C-   First release with explicit veto code
C-
C-   Revision 2.2.3.1  1995/08/25  01:56:24  hobbs
C-   Check for nonblank veto type
C-
C-   Revision 2.2.2.1  1995/08/24  13:51:34  hobbs
C-   Trivial fix to error message for blank (NULL) veto types
C-
C-   Revision 2.2.1.1  1995/08/24  02:37:10  hobbs
C-   Get proper revision/log numering in comments (RCS fixup)
C-
C-
C-   Revision 2.1  1995/08/09  02:59:24  hobbs
C-   Minor mods: version numbering, informational messasges
C-
C-   Revision 2.0  1995/08/09  02:38:34  hobbs
C-   Introduced with complete rewrite of MUJETS_MU_SELECT.  Computes and
C-   tests all possible isolation combinations.
C-
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:MUJETS_MU_SELECT.PARAMS'
C- Function Parameters
      INTEGER LPMUO,IMUON,ITYPE,NCOPY
      REAL    DRCLOSE_NEW,ISOLCOPY(*)
C- RCP parameters.
      INTEGER NTYPES,ISOLTYPE(NTYPES_MAX)
      REAL    ISOLMIN(NTYPES_MAX),ISOLMAX(NTYPES_MAX),JET_ET_MIN
      CHARACTER*8 JET_TYPE
      LOGICAL DO_MUON_VTX_CHECK
C- Temporary storage.
      LOGICAL FIRST,MUJETS_STORE_ISOL
      REAL    DETA,DPHI,DRR,DEDIFF,DELTAE,DELTAR,PT,ZV1,ZV_MUON
      REAL    PT_NEW,ETA_NEW,THETA_NEW,ETA_JET,PNEW(4),PTR,PTOT(4)
      REAL    COSANG,PTREL_NEW
      INTEGER IERR,LJET,LCLOSE(NMAX),LCLOSE_PRIME(NMAX)
      INTEGER NMUONS,NJETS,LJETS(NMAX),I,J,IER,LTEMP,IVTX
      LOGICAL USE_RECALC
      CHARACTER*(*) ERRNAME,ERRTXT*80
      PARAMETER(ERRNAME='MUJETS_ISOLATION')
C- External functions
      INTEGER GZPMUO,GZVERT,LENOCC
      REAL    DETA_TO_PETA,PETA_TO_DETA,VDOTN
      LOGICAL MUJETS_GET_ISOLATION,MUJETS_MU_GETPAR
      LOGICAL MUJETS_GET_RECALC_ISOLATION,MUJETS_ISOLATION_FREE
C- Long-term internal storage
      REAL    DRCLOSE(NMAX),EDIFF(NMAX),DRCLOSE_PRIME(NMAX)
      REAL    PTREL(NMAX),PTREL_PRIME(NMAX)
      INTEGER OLDRUN,OLDEVT,LINKA(2),MYLINKS(NMAX),MUVTX(NMAX)
      COMMON/MUJETS_ISOL_COMMON/LINKA,MYLINKS,NJETS,DRCLOSE,EDIFF,
     >  PTREL,LCLOSE,DRCLOSE_PRIME,LCLOSE_PRIME,PTREL_PRIME,MUVTX
      DATA FIRST/.TRUE./,OLDRUN/0/,OLDEVT/0/
      SAVE FIRST,NTYPES,ISOLTYPE,ISOLMIN,ISOLMAX,OLDRUN,OLDEVT
      SAVE JET_TYPE,JET_ET_MIN,DO_MUON_VTX_CHECK
C-----------------------------------------------------------------------
C
      IF(FIRST) THEN
C
        WRITE(ERRTXT,2001) ERRNAME,'2.5'
 2001   FORMAT(A,' V',A)
        CALL ERRMSG(ERRTXT(1:LENOCC(ERRTXT)),ERRNAME,' ','I')
C
        CALL EZPICK('MUJETS_MU_SELECT_RCP')
        CALL EZERR(IERR)
        IF( IERR.NE.0 )
     >    CALL ERRMSG('NO_RCP_BANK','MUJETS_ISOLATION',' ','F')
C
        CALL EZGETA_i('ISOLATION_ALG',0,NTYPES_MAX,0,NTYPES,IERR)
        CALL EZGETA_iarr('ISOLATION_ALG',1,NTYPES,1,ISOLTYPE,IER)
        IF(IER.EQ.0) CALL EZGETA('ISOLATION_MIN',1,NTYPES,1,ISOLMIN,IER)
        IF(IER.EQ.0) CALL EZGETA('ISOLATION_MAX',1,NTYPES,1,ISOLMAX,IER)
        IF(IER.EQ.0) CALL EZGET('JET_ET_MIN',JET_ET_MIN,IER)
        IF(IER.EQ.0) CALL EZGET_l('DO_MUON_VTX_CHECK',
     >   DO_MUON_VTX_CHECK,IER)
        IF(IER.EQ.0) CALL EZ_GET_STRING('JET_TYPE',JET_TYPE,J,ERRNAME)
        IF(IER.NE.0) THEN
          CALL ERRMSG('MISSING_RCP_PARAM',ERRNAME,' ','F')
          MUJETS_ISOLATION=.FALSE.
          GOTO 999
        ENDIF
C
        FIRST=.FALSE.
C
      ENDIF
C
C- If it's a new event, loop over all muons and compute dR(min) with 
C- respect to closest jet.
C
      IF( IQ(LHEAD+9).NE.OLDEVT .OR. IQ(LHEAD+6).NE.OLDRUN ) THEN
        CALL MZLINT(IXCOM,'/LMUJETS/',LINKA,MYLINKS,MYLINKS(NMAX))
        OLDEVT=IQ(LHEAD+9)
        OLDRUN=IQ(LHEAD+6)
        NMUONS=0
        CALL VZERO(MYLINKS,NMAX)
        CALL VZERO(LCLOSE,NMAX)
        CALL VZERO(LCLOSE_PRIME,NMAX)
        CALL VZERO(LJETS,NMAX)
        CALL GTSLINK(JET_TYPE,NMAX,NJETS,LJETS)
        LPMUO=GZPMUO(0)
        DO WHILE( LPMUO.GT.0 ) 
C    Check jet/muon variables
          NMUONS=NMUONS+1
          MYLINKS(NMUONS)=LPMUO
          IVTX=IQ(LPMUO+54)
          MUVTX(NMUONS)=IVTX
          DRCLOSE(NMUONS)=999.9
          DO 20 J=1,NJETS
            LJET=LJETS(J)
            DETA=Q(LJET+9)-Q(LPMUO+16)
            DPHI=ABS(Q(LJET+8)-Q(LPMUO+17))
            IF( DPHI.GT.3.1416 ) DPHI=6.2832-DPHI
            DELTAR=SQRT(DETA**2+DPHI**2)
            PT=Q(LJET+6)
            IF( DELTAR.LT.DRCLOSE(NMUONS) .AND. PT.GE.JET_ET_MIN ) THEN
              DRCLOSE(NMUONS)=DELTAR
              LCLOSE(NMUONS)=J
            ENDIF
 20       CONTINUE
          EDIFF(NMUONS)=Q(LPMUO+35)-Q(LPMUO+34)
          IF( NJETS.EQ.0 ) THEN
            PTREL(NMUONS) = -2.0
            LCLOSE(NMUONS)= 0
          ELSE
            LJET=LJETS(LCLOSE(NMUONS))
            CALL VADD(Q(LPMUO+10),Q(LJET+2),PTOT,3)
            IF( (PTOT(1).EQ.0.AND.PTOT(2).EQ.0.AND.PTOT(3).EQ.0) .OR.
     >      (Q(LPMUO+10).EQ.0.AND.Q(LPMUO+11).EQ.0.AND.Q(LPMUO+12).EQ.0)
     >       ) THEN
              PTREL(NMUONS)=-2.0
              WRITE(ERRTXT,9001) PTOT,Q(LPMUO+10)
 9001         FORMAT(' P(mu)=',3(F7.0),' P(jet)=',3(F7.0),' Protecting')
              CALL ERRMSG('BAD_VECTOR',ERRNAME,ERRTXT,'E')
            ELSE
              COSANG=VDOTN(PTOT,Q(LPMUO+10),3)
              PTREL(NMUONS)=Q(LPMUO+13)*SQRT(1.0-COSANG**2)
            ENDIF
          ENDIF
          DRCLOSE_PRIME(NMUONS)=DRCLOSE(NMUONS)
          LCLOSE_PRIME(NMUONS)=LCLOSE(NMUONS)
          PTREL_PRIME(NMUONS)=PTREL(NMUONS)

C    If muon from secondary, recompute eta/pt/drisol/ptrel with consistant
C    event vertices.

          IF( IVTX.GT.1 ) THEN
            DRCLOSE_PRIME(NMUONS)=999.9
            LCLOSE_PRIME(NMUONS)=0
            CALL RECALC_MUON(LPMUO,PT_NEW,ETA_NEW,THETA_NEW,IER)
            PNEW(1)=PT_NEW*COS(Q(LPMUO+17))
            PNEW(2)=PT_NEW*SIN(Q(LPMUO+17))
            PNEW(3)=PT_NEW/SIN(MIN(MAX(THETA_NEW,0.04),3.10))
            PNEW(4)=SQRT(PT_NEW**2+PNEW(3)**2)
            DO 30 J=1,NJETS
              LJET=LJETS(J)
              DETA=Q(LJET+9)-ETA_NEW
              DPHI=ABS(Q(LJET+8)-Q(LPMUO+17))
              IF( DPHI.GT.3.1416 ) DPHI=6.2832-DPHI
              DELTAR=SQRT(DETA**2+DPHI**2)
              PT=Q(LJET+6)
              IF( DELTAR.LT.DRCLOSE_PRIME(NMUONS) 
     >                        .AND. PT.GE.JET_ET_MIN ) THEN
                DRCLOSE_PRIME(NMUONS)=DELTAR
                LCLOSE_PRIME(NMUONS)=J
              ENDIF
 30         CONTINUE
            DO 40 J=1,NJETS
              LJET=LJETS(J)
              ZV1=Q(GZVERT(1)+5)
              ZV_MUON=Q(GZVERT(IVTX)+5)
              ETA_JET=PETA_TO_DETA(Q(LJET+9),ZV1)
              ETA_JET=DETA_TO_PETA(ETA_JET,ZV_MUON)
              DETA=Q(LPMUO+16)-ETA_JET
              DPHI=ABS(Q(LJET+8)-Q(LPMUO+17))
              IF( DPHI.GT.3.1416 ) DPHI=6.2832-DPHI
              DELTAR=SQRT(DETA**2+DPHI**2)
              PT=Q(LJET+6)
              IF( DELTAR.LT.DRCLOSE_PRIME(NMUONS) 
     >                           .AND. PT.GE.JET_ET_MIN ) THEN
                DRCLOSE_PRIME(NMUONS)=DELTAR
                LCLOSE_PRIME(NMUONS)=J
              ENDIF
 40         CONTINUE
C-       Compute PTREL wrt primary if muon on secondary
            IF( NJETS.GT.0 ) THEN
              LJET=LJETS(LCLOSE_PRIME(NMUONS))
              CALL VADD(Q(LJET+2),PNEW,PTOT,3)
              IF( (PTOT(1).EQ.0.AND.PTOT(2).EQ.0.AND.PTOT(3).EQ.0) .OR.
     >        (PNEW(1).EQ.0.AND.PNEW(2).EQ.0.AND.PNEW(3).EQ.0) ) THEN
                PTREL_PRIME(NMUONS)=-2.0
                WRITE(ERRTXT,9001) PTOT,PNEW
                CALL ERRMSG('BAD_VECTOR',ERRNAME,ERRTXT,'E')
              ELSE
                COSANG=VDOTN(PNEW,PTOT,3)
                PTREL_PRIME(NMUONS)=PNEW(4)*SQRT(1.-COSANG**2) 
              ENDIF
            ENDIF
          ENDIF
          LPMUO=LQ(LPMUO)
        ENDDO
      ENDIF

C- Escape if just performing isolation calculations...

      IF( IMUON.EQ.0 ) GOTO 999

C- Apply the isolation...

      MUJETS_ISOLATION=.TRUE.
      IF( ISOLTYPE(ITYPE).EQ.0 ) GOTO 999   ! No isolation.   Always passes.
C
      IF( ISOLTYPE(ITYPE).EQ.1 ) THEN   ! dR only
        USE_RECALC = DO_MUON_VTX_CHECK .AND. MUVTX(IMUON).GT.1 
        DELTAR=DRCLOSE(IMUON)
        IF( USE_RECALC ) DELTAR=MIN(DELTAR,DRCLOSE_PRIME(IMUON))
        IF( DELTAR.LT.ISOLMIN(ITYPE) ) MUJETS_ISOLATION=.FALSE.
        IF( DELTAR.GT.ISOLMAX(ITYPE) ) MUJETS_ISOLATION=.FALSE.
      ELSEIF( ISOLTYPE(ITYPE).EQ.2 ) THEN
        IF( EDIFF(IMUON).LT.ISOLMIN(ITYPE) ) MUJETS_ISOLATION=.FALSE.
        IF( EDIFF(IMUON).GT.ISOLMAX(ITYPE) ) MUJETS_ISOLATION=.FALSE.
      ELSEIF( ISOLTYPE(ITYPE).EQ.3 ) THEN
        PTR = PTREL(IMUON)
        IF( USE_RECALC ) PTR = MIN(PTR,PTREL_PRIME(IMUON))
        IF( PTR.LT.ISOLMIN(ITYPE) ) MUJETS_ISOLATION=.FALSE.
        IF( PTR.GT.ISOLMAX(ITYPE) ) MUJETS_ISOLATION=.FALSE.
      ELSE
        CALL ERRMSG('UNKNOWN_ISOLATION',ERRNAME,'Pass muon','W')
      ENDIF
      GOTO 999
C

C-----------------------------------------------------------------------
      ENTRY MUJETS_MU_GETPAR(LTEMP,DRR,DEDIFF)
C-----------------------------------------------------------------------
      MUJETS_MU_GETPAR=.FALSE.
      DO 50 I=1,NMUONS
        IF( LTEMP.NE.MYLINKS(I) ) GOTO 50
        MUJETS_MU_GETPAR=.TRUE.
        DRR=DRCLOSE(I)
        DEDIFF=EDIFF(I)
        GOTO 999
 50   CONTINUE
      GOTO 999

C-----------------------------------------------------------------------
      ENTRY MUJETS_GET_ISOLATION(LTEMP,ISOLCOPY,NCOPY)
C-----------------------------------------------------------------------
      MUJETS_GET_ISOLATION=.FALSE.
      IER=1
      DO 60 I=1,NMUONS
        IF( LTEMP.NE.MYLINKS(I) ) GOTO 60
        MUJETS_GET_ISOLATION=.TRUE.
        ISOLCOPY(1)=DRCLOSE(I)
        ISOLCOPY(2)=EDIFF(I)
        ISOLCOPY(3)=PTREL(I)
        NCOPY=NISOL_MAX
        GOTO 999
 60   CONTINUE
      GOTO 999

C-----------------------------------------------------------------------
      ENTRY MUJETS_GET_RECALC_ISOLATION(LTEMP,DRCLOSE_NEW,PTREL_NEW)
C-----------------------------------------------------------------------
C-
C-  Return a copy of the isolation dR if muon recomputed at a different
C-  vertex.
C
C-----------------------------------------------------------------------
C
      MUJETS_GET_RECALC_ISOLATION = .FALSE.
      DRCLOSE_NEW = -999.9
      DO 70 I=1,NMUONS
        IF( MYLINKS(I).NE.LTEMP ) GOTO 70
        MUJETS_GET_RECALC_ISOLATION = .TRUE.
        DRCLOSE_NEW = DRCLOSE_PRIME(I)
        PTREL_NEW = PTREL_PRIME(I)
        GOTO 999
 70   CONTINUE
      GOTO 999

C-----------------------------------------------------------------------
      ENTRY MUJETS_ISOLATION_FREE
C-----------------------------------------------------------------------
C-
C-  Release the protected link area.
C-
C-----------------------------------------------------------------------
C
      MUJETS_ISOLATION_FREE=.TRUE.
      LINKA(1)=0
      GOTO 999
C
 999  RETURN
      END

