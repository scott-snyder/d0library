      SUBROUTINE RGS_CREATE_NTUPLE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book RGSEARCH ntuple.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls:
C-
C-   Created  28-APR-1995   Harrison B. Prosper
C-   Updated   1-MAY-1995   Harrison B. Prosper
C-   Updated  29-JUN-1995   Jeffrey E. McDonald
C-   Updated   5-JUL-1995   Harrison B. Prosper, Jeffrey E. McDonald
C-   Updated  11-JUL-1995   Harrison B. Prosper   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:RGSCOM.INC'
C----------------------------------------------------------------------
      INTEGER LREC
      PARAMETER( LREC   = 1024 )
      INTEGER NPRIME
      PARAMETER( NPRIME = 1024 )
C
      INTEGER I,J,K,L,JJ,NTAGS,STATUS,ICUT,IFILE
C
      INTEGER NOPT,OPT_NTUPLE_ID,NB
      PARAMETER( NOPT = 1000 )
      REAL    XT(MAXTAG), EFF, XXS(NOPT),DXS(NOPT), XSIG,XBKG, DXSIG
      REAL    SIGMAS, BMAX
C
      LOGICAL EZERROR, KEEP
C
      CHARACTER*80  TITLE
      CHARACTER*32  TAG(MAXTAG)
      CHARACTER*4   CHOPT
C----------------------------------------------------------------------
C
C ****  Get some RCP stuff
C
      CALL EZPICK('RGSEARCH_RCP')
      IF ( EZERROR(STATUS) ) THEN
        CALL ERRMSG('NO_RCP_BANK',
     &    'RGS_CREATE_NTUPLE','No RGSEARCH.RCP','F')
      ENDIF
      CALL EZ_GET_CHARS('PATTERNS_TITLE',I,TITLE,STATUS)
      IF ( STATUS .NE. 0 ) THEN
        TITLE = 'RGSEARCH V1.0'
      ENDIF
      CALL EZGET('OPTIMIZE_BINS',NB,STATUS)
      IF ( STATUS .NE. 0 ) THEN
        NB = NOPT
      ENDIF
      CALL EZGET('OPTIMIZE_MAX_BKG',BMAX,STATUS)
      IF ( STATUS .NE. 0 ) THEN
        BMAX = 100.0
      ENDIF
      CALL EZGET('OPTIMIZE_ID',OPT_NTUPLE_ID,STATUS)
      IF ( STATUS .NE. 0 ) THEN
        OPT_NTUPLE_ID = 2
      ENDIF
      CALL EZGET('OPTIMIZE_SIGMAS',SIGMAS,STATUS)
      IF ( STATUS .NE. 0 ) THEN
        SIGMAS = 2.0
      ENDIF
      CALL EZRSET
C
C ****  Define tag names;
C ****  NLABO is the number of output labels. This Number is the same as
C ****  NFILE since there is one output label per file.
C
      CALL DGN_GET_TAGS(MAXIN,NTAGS,TAG)
C
      DO I =  1, NFILE
C
C ****  TAG FOR COUNT
C
        NTAGS = NTAGS + 1
        CALL WORD(LABO(I),J,K,L)
        TAG(NTAGS) = LABO(I)(J:K)
C
C ****  TAG FOR EFFICIENCY
C
        NTAGS = NTAGS + 1
        TAG(NTAGS) = 'eff_'//LABO(I)(J:K)
C
C ****  TAGS FOR (AVE,SIG) FOR EACH FIELD
C
        L = MIN(3,L)
        DO JJ = 1, NFIELD
          NTAGS = NTAGS + 1
          TAG(NTAGS) = 'a'//LABO(I)(1:L)//TAG(JJ)
          NTAGS = NTAGS + 1
          TAG(NTAGS) = 's'//LABO(I)(1:L)//TAG(JJ)
        ENDDO
      ENDDO
C
C ****  OPEN RZ-FILE
C
      CALL GTUNIT(NETID,OUNIT,STATUS)
      CHOPT = 'N'
      CALL HCDIR('//PAWC',' ')
      CALL HROPEN (OUNIT,'OUTPUT',OUTFILE, CHOPT,LREC,STATUS)
C
C ****  BOOK NTUPLE
C
      CALL HBOOKN (OUT_NTUPLE_ID,TITLE,NTAGS,'OUTPUT',NPRIME,TAG)
      IF(OPT_NTUPLE_ID.GT.0)
     &  CALL HBOOKN (OPT_NTUPLE_ID,
     &  TITLE(1:20)//' OPT',NTAGS,'OUTPUT',NPRIME,TAG)
C
C ****  FILL "UNOPTIMIZED" NTUPLE
C
      CALL VZERO(XXS,NB)
      DO ICUT = 1, NCUT
C
C ****  NFIELD = NSELECT + FIELDS NOT SELECTED
C
        CALL UCOPY(PATTERN_CUT(1,ICUT),XT(1),NFIELD)
        J    = 0
        XBKG = 0.0
        XSIG = 0.0        !SIGNAL COUNT
        DXSIG= 0.0        !SIGMA ON SIGNAL COUNT
C
        DO IFILE = 1, NFILE
C
C ****  COUNT
C
          J = J + 1
          XT(NFIELD+J) = PATTERN_OUT(IFILE,ICUT)   ! Counts
C
C ****  RELATIVE EFFICIENCY
C
          J = J + 1
          EFF = PATTERN_OUT(IFILE,ICUT)/TOT_IN(IFILE)
          XT(NFIELD+J) = EFF
C
C ****  AVERAGES AND SIGMAS
C
          DO JJ = 1, NFIELD
            J = J + 1
            XT(NFIELD+J) = PATTERN_AVE(JJ,IFILE,ICUT)
            J = J + 1
            XT(NFIELD+J) = PATTERN_SIG(JJ,IFILE,ICUT)
          ENDDO
C
          XBKG = XBKG +  BKGW(IFILE)*PATTERN_OUT(IFILE,ICUT)
          XSIG = XSIG +  SIGW(IFILE)*PATTERN_OUT(IFILE,ICUT)
          DXSIG= DXSIG+ (SIGW(IFILE)**2)*PATTERN_OUT(IFILE,ICUT)*(1-EFF)
        ENDDO
        DXSIG = SQRT(DXSIG)
C
C ****  FIND BIN IN WHICH XSIG LIES
C ****  FIND MAXIMUM XSIG FOR THE kth BIN
C
        K = NB*XBKG/BMAX+1
        IF(K.LE.NB) THEN
          IF(XSIG.GT.XXS(K)) THEN
            XXS(K) = XSIG
            DXS(K) = DXSIG
          ENDIF
        ENDIF
C
C ****  WRITE TO 1st NTUPLE
C
        CALL HCDIR('//OUTPUT',' ')
        CALL HCDIR('//PAWC',' ')
        CALL HFN(OUT_NTUPLE_ID,XT)
      ENDDO
C
C ****  NOW LOOP OVER CUTS AGAIN SELECTING THOSE WITHIN 2 SIGMA OF
C ****  THE OPTIMAL BOUNDARY.
C
      WRITE(6,'('' Deriving optimal boundary...'')')
      DO ICUT = 1, NCUT
C
C ****  1. COMPUTE SIGNAL COUNT (XSIG)
C ****  2. FIND WHICH BIN IT LIES IN
C ****  3. CHECK IF CURRENT XSIG IS WITHIN 2*DXS(K) OF THE
C ****  OPTIMAL BOUNDARY.
C
        XBKG = 0.0
        XSIG = 0.0        !SIGNAL COUNT
        DO IFILE = 1, NFILE
          XBKG = XBKG +  BKGW(IFILE)*PATTERN_OUT(IFILE,ICUT)
          XSIG = XSIG +  SIGW(IFILE)*PATTERN_OUT(IFILE,ICUT)
        ENDDO
C
C ****  FIND IN WHICH BIN XSIG LIES
C
        KEEP = .FALSE.
        K = (XBKG/BMAX)*NB + 1
        IF(K.LE.NB) THEN
          IF(XSIG.GE.(XXS(K)-SIGMAS*DXS(K))) THEN
            KEEP = .TRUE.
          ENDIF
        ENDIF
C
        IF ( KEEP ) THEN
C
C ****  THIS CUT LIES NEAR OPTIMAL BOUNDARY; SO SAVE IT TO
C ****  OPTIMAL BOUNDARY NTUPLE.
C
          CALL UCOPY(PATTERN_CUT(1,ICUT),XT(1),NFIELD)
          J    = 0
          DO IFILE = 1, NFILE
C
C ****  COUNT
C
            J = J + 1
            XT(NFIELD+J) = PATTERN_OUT(IFILE,ICUT)   ! Counts
C
C ****  RELATIVE EFFICIENCY
C
            J = J + 1
            XT(NFIELD+J) = PATTERN_OUT(IFILE,ICUT)/TOT_IN(IFILE)
C
C ****  AVERAGES AND SIGMAS
C
            DO JJ = 1, NFIELD
              J = J + 1
              XT(NFIELD+J) = PATTERN_AVE(JJ,IFILE,ICUT)
              J = J + 1
              XT(NFIELD+J) = PATTERN_SIG(JJ,IFILE,ICUT)
            ENDDO
          ENDDO
C
C ****  WRITE TO OPTIMAL BOUNDARY NTUPLE
C
          CALL HFN(OPT_NTUPLE_ID,XT)
        ENDIF
      ENDDO
C
C ****  CLOSE RZ FILE
C
      J = 0
      CALL HCDIR('//PAWC',' ')
      CALL HCDIR('//OUTPUT',' ')
      CALL HROUT(0,J,' ')
      CALL HREND('OUTPUT')
      CLOSE(OUNIT)
      CALL RLUNIT(NETID,OUNIT,STATUS)
C
  999 RETURN
      END
