C------------------------------------------------------------------------
      LOGICAL FUNCTION MUJETS_MU_SELECT
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Apply full muon selection to muons
C-    selected in PARTICLE_SELECT.  Isolation is applied in MUJETS_ISOLATION
C-    called by this routine... 
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  25-Apr-1994   John D. Hobbs
C-   Updated  11-DEC-1994   Meenakshi Narain   add cut on jet_et for isolation
C-                              fix the output value for DR
C-   Modified 27-Jul-1995   John D. Hobbs - full rewrite to allow run
C-     dependence.  Oh, this gets ugly...
C-   Modified 22-Aug-1995   John D. Hobbs - add veto code.
C-
C-   mujets_mu_select.for,v
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
C-   Revision 2.1  1995/08/09  02:59:24  hobbs
C-   Minor mods: version numbering, informational messasges
C-
C-   Revision 2.0  1995/08/09  02:26:24  hobbs
C-   Major rewrite to allow run dependence
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INCLUDE 'D0$PARAMS:MUJETS_MU_SELECT.PARAMS'
C- Particle type definitions.
      INTEGER NTYPES,FIRST_RUN(NTYPES_MAX),LAST_RUN(NTYPES_MAX)
      CHARACTER*8 PARENT_TYPE(NTYPES_MAX),PARTICLE_TYPE(NTYPES_MAX)
      CHARACTER*8 VETO_TYPE(NTYPES_MAX)
      REAL MONTE_CARLO(NTYPES_MAX)
C- Storage for good muons.
      INTEGER NMUONS
C- Temporary internal variables.
      INTEGER LPMUO,PASSED(NMAX),IRUN,I,J,LTEMP,IERR,IER,LPARH,IDROP
      INTEGER IERR_FAILED,NMAX_FAILED,NCOPY,NJETS,LJETS(NMAX)
      INTEGER LVETO(NMAX),IVETO
      LOGICAL LMC,LDUMMY
      REAL PTYPE(NTYPES_MAX),DRR,DEDIFF,PARS(NTYPES_MAX)
      CHARACTER*(*) ERRNAME,ERRTXT*80
      PARAMETER(ERRNAME='MUJETS_MU_SELECT')
C- Link storage.
      INTEGER N_OF_TYPE(NTYPES_MAX),LINK_NUM(NMAX,NMAX),QUAD
      INTEGER VETO_INDEX(NTYPES_MAX)
      REAL    DR(NMAX),EDIFF(NMAX)
      SAVE    LINK_NUM,DR,EDIFF
C- External functions
      LOGICAL MUJETS_ISOLATION,MUJETS_MU_SELECT_FIN
      LOGICAL GOOD_PARTICLE
      INTEGER JBIT,GZPARH,GZPMUO,LENOCC
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      SAVE FIRST,PARENT_TYPE,PARTICLE_TYPE,VETO_TYPE,MONTE_CARLO
      SAVE FIRST_RUN,LAST_RUN,VETO_INDEX
C-----------------------------------------------------------------------
C
      IF(FIRST) THEN
C
        WRITE(ERRTXT,2001) ERRNAME,'2.5'
 2001   FORMAT(A,' V',A)
        CALL ERRMSG(ERRTXT(1:LENOCC(ERRTXT)),ERRNAME,' ','I')
C
        IERR=0
        CALL INRCP('MUJETS_MU_SELECT_RCP',IERR)
        IF( IERR.NE.0 ) CALL ERRMSG('MUJETS_MU_SELECT_NORCP',ERRNAME,
     >      'RCP not found','F')
C
        CALL EZGETA('FIRST_RUN',0,NTYPES_max,0,NTYPES,IERR)
        CALL EZGETA('FIRST_RUN',1,NTYPES,1,FIRST_RUN,IERR)
        NMAX_FAILED=MAX(NTYPES,NTYPES_MAX)
        IF(IERR.EQ.0)CALL EZGETA('LAST_RUN',1,NTYPES,1,LAST_RUN,IERR)
        NMAX_FAILED=MAX(NTYPES,NTYPES_MAX)
        IF(IERR.EQ.0)CALL EZ_GET_CHARS('PARTICLE_TYPE',NTYPES,
     >    PARTICLE_TYPE,IERR)
        IF(IERR.EQ.0)CALL EZ_GET_CHARS('PARENT_TYPE',NTYPES,
     >    PARENT_TYPE,IERR)
        IF(IERR.EQ.0)CALL EZ_GET_CHARS('VETO_TYPE',NTYPES,
     >    VETO_TYPE,IERR)
        IF(IERR.EQ.0)CALL EZGETA('MONTE_CARLO_WEIGHT',1,NTYPES,1,
     >    MONTE_CARLO,IERR)
        NMAX_FAILED=MAX(NTYPES,NTYPES_MAX)
C
        IF( IERR.NE.0 ) THEN
          CALL ERRMSG('MISSING_RCP_PARAM',ERRNAME,' ','F')
          MUJETS_MU_SELECT=.FALSE.
          GOTO 999
        ENDIF
C
        IF( NMAX_FAILED.GT.NMAX ) THEN
          CALL ERRMSG('TOO_MANY_RCP_PARAMS',ERRNAME,' ','F')
          MUJETS_MU_SELECT=.FALSE.
          GOTO 999
        ENDIF
C
        CALL VZERO(VETO_INDEX,NTYPES_MAX)
        DO I=1,NTYPES
        DO J=1,NTYPES
          IF(J.NE.I.AND.VETO_TYPE(I).EQ.PARTICLE_TYPE(J))VETO_INDEX(I)=J
        ENDDO
        ENDDO
C
        FIRST=.FALSE.
      ENDIF
C
      MUJETS_MU_SELECT=.TRUE.
      CALL VZERO(N_OF_TYPE,NTYPES_MAX)
      CALL VZERO(PASSED,NMAX)
C
C- Check limit on PMUO count...
C
      LPARH=GZPARH()
      IF( LPARH.LE.0 ) THEN
        CALL ERRMSG('NO_PARH_FOUND',ERRNAME,' ','W')
        RETURN
      ENDIF
C
      NMUONS = IQ(LPARH+2)
      IF( NMUONS.GT.NMAX ) THEN
        CALL ERRMSG('MUON_COUNT_EXCEEDED',ERRNAME,' ','W')
        RETURN
      ENDIF
C
C- Compute the isolation for all muons
C
      LDUMMY = MUJETS_ISOLATION(0,NMUONS)
C
C- Loop over all known particle types for all  muons.
C
      IRUN = IQ(LHEAD+6)
      LMC = IQ(LHEAD+1).GE.1005   ! .TRUE. ==> Data is Monte Carlo
      NMUONS=0
      LPMUO=GZPMUO(0)
      DO WHILE( LPMUO.GT.0 )
        NMUONS=NMUONS+1
        DO 10 I=1,NTYPES
          IF( .NOT.GOOD_PARTICLE(LPMUO,PARENT_TYPE(I)) ) GOTO 10
          IF(  LMC .AND. MONTE_CARLO(I).NE.1.0 ) GOTO 10
          IF( IRUN.LT.FIRST_RUN(I) .AND. .NOT.LMC  ) GOTO 10
          IF( IRUN.GT.LAST_RUN(I) .AND. .NOT.LMC  ) GOTO 10
          IF( .NOT.MUJETS_ISOLATION(NMUONS,I) ) GOTO 10
C-     This is a good muon of this type...
          CALL SBIT1(PASSED(NMUONS),I)
          N_OF_TYPE(I)=N_OF_TYPE(I)+1
          CALL GSLINK(PARTICLE_TYPE(I),LINK_NUM(N_OF_TYPE(I),I))
          LSLINK(LINK_NUM(N_OF_TYPE(I),I))=LPMUO
 10     CONTINUE
        LPMUO = LQ(LPMUO)
      ENDDO
C
C- Apply vetos...
C
      NMUONS=0
      LPMUO=GZPMUO(0)
      DO WHILE( LPMUO.GT.0 ) 
        NMUONS=NMUONS+1
        DO 20 I=1,NTYPES
C-    Is a veto needed?
          IF( VETO_INDEX(I).EQ.0 ) GOTO 20
          IF( .NOT.GOOD_PARTICLE(LPMUO,PARTICLE_TYPE(I)) ) GOTO 20
          IF( .NOT.GOOD_PARTICLE(LPMUO,VETO_TYPE(I)) ) GOTO 20
C-    Do the veto.
C-    Find the index in the zlinka area which contains this particle
          CALL GTSLINK(PARTICLE_TYPE(I),NMAX,J,LVETO)
          IF( J.NE.N_OF_TYPE(I) ) THEN
            WRITE(ERRTXT,*) 'Inconsitant link counts for type ',
     >         PARTICLE_TYPE(I)
            CALL ERRMSG('BAD_LINK_COUNT',ERRNAME,ERRTXT,'F')
          ENDIF
          IDROP=0
          DO J=1,N_OF_TYPE(I)
            IF( LPMUO.EQ.LVETO(J) ) IDROP=J
          ENDDO
C-    Release the link and adjust the local information.
          IF( IDROP.EQ.0 ) THEN
            WRITE(ERRTXT,*) 'Could not find current particle (',
     >         VETO_TYPE(I),') in protected link area.  LOGIC ERROR'
            CALL ERRMSG('MISSING_LINK',ERRNAME,ERRTXT,'F')
            GOTO 20
          ELSE
            CALL RSLINK(PARTICLE_TYPE(I),LINK_NUM(IDROP,I))
            N_OF_TYPE(I)=N_OF_TYPE(I)-1
            DO J=IDROP,N_OF_TYPE(I)
             LSLINK(LINK_NUM(J,I))=LSLINK(LINK_NUM(J+1,I))
             LINK_NUM(J,I)=LINK_NUM(J+1,I)
            ENDDO
          ENDIF
 20     CONTINUE
        LPMUO=LQ(LPMUO)
      ENDDO
C
      GOTO 999

C-----------------------------------------------------------------------
      ENTRY MUJETS_MU_SELECT_FIN
C----------------------------------------------------------------------
      MUJETS_MU_SELECT_FIN=.TRUE.
      DO I=1,NTYPES
        DO J=1,N_OF_TYPE(I)
          CALL RSLINK(PARTICLE_TYPE(I),LINK_NUM(J,I))
        ENDDO
        N_OF_TYPE(I)=0
      ENDDO
      CALL MUJETS_ISOLATION_FREE
      GOTO 999
 999  CONTINUE
      RETURN
      END
