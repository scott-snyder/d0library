      LOGICAL FUNCTION ESUM_COMPARE()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Compare required OBJECTS in two ESUM banks;
C-                          Calculate efficiency, rejection, etc., of one
C-                            bank w.r.t other;
C-                          Provide hostograms and statistical information
C-                            for MATCHed and UN-MATCHed OBJECTs.
C-
C-                 Calls :  GTESUM
C-                          GTESUM_COUNTS
C-                          GTESUM_MATCH
C-                          ESUM_INIT
C-
C-   Inputs  :              None
C-   Outputs :              Host of Histograms
C-   Controls:              ESUM_COMPARE.RCP File required
C-
C-   Created  27-MAR-1992   M.V.S. Rao
C-   Updated  27-APR-1992   Serban D. Protopopescu  (call ISAE_ESUM)
C-   Updated  26-JUN-1992   M.V.S. Rao  (Histogram titles fixed)
C-   Updated  29-JUN-1992   Meenakshi Narain (convert to V3 compatibility)
C-   Updated   1-JUL-1992   Meenakshi Narain  (fix eta binning for TRGR and
C-                                        FILT objects)
C-   Updated   5-JUL-1992   Meenakshi Narain
C-                          Add option to
C-                          a) write event list for unmatched objects
C-                          b) DUMP events with objects to DST
C-   Updated   6-JUL-1992   Meenakshi Narain
C-                          Add option to require a match of the REF and DTA
C-                          objects to a third object REQ.
C-   Updated  19-AUG-1992   sss - compile on ibm
C-   Updated  10-NOV-1993   M.V.S. Rao  (Fixed some trivial FLINT conflicts) 
C-   Updated  29-Dec-1995   sss - compile with g77.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$PARAMS:ESUM.PARAMS'
      CHARACTER*4 REF,DTA,REQ
      CHARACTER*(80) MSG,TITLE,EVLST_FILE, EVENTS_FILE
      CHARACTER*10 OB_NAME( ID_ALL : LAST_TYPE ),ARRAY_NAME
      CHARACTER*24 HBOOK_DIR(50),CHDIR
      CHARACTER*2 CM
      INTEGER NPAIRS,NUM_PAIRS,IREF,IDTA,IREQ
      INTEGER ID_OBJ_REF(50),ID_OBJ_DTA(50),ID_OBJ_REQ(50)
      INTEGER NMX,NMC,MTM(1600,2),IER,I,NUM_OBJ_TYPE,IER_SUB
      INTEGER PMX,PMC,PMAT(1600,2),SMX,SMC,SMAT(1600,2)
      INTEGER IDH,IDHN,ID1,ID2,ID3,ID4
      INTEGER NFOUND_REF( ID_ALL : LAST_TYPE )
      INTEGER NFOUND_DTA( ID_ALL : LAST_TYPE ),J,K,L
      INTEGER NFOUND_REQ( ID_ALL : LAST_TYPE )
      INTEGER IDO(2),IDO3
      INTEGER ISTR(50),ISTD(50),LNGR
      INTEGER IENR(50),IEND(50),LNGD
      INTEGER ISTC(50),IENC(50),LNGC,LEN
      INTEGER NID,IDPAR,N,IVAL(7),TYPE(7),NVAL
      INTEGER ETA_BINS, PHI_BINS
      INTEGER EV_FLAG(50),EVENT(100),RUN,RUNNO,RUNO,EVONUM,NEVT
      INTEGER WUNIT,PUNIT
      INTEGER STATUS, NCHARS,TRNLNM
      INTEGER FLAG,FLAGR,FLAGD,FLAGC
      REAL OB_RESOLVE( ID_ALL : LAST_TYPE )
      REAL OB_THRESH ( ID_ALL : LAST_TYPE )
      REAL    CSP,DTHETA,ETA,ET,ETA_DET,THETA,PHI
      REAL    ETTHR_REF(50),ETTHR_DTA(50),ETTHR_REQ(50)
      REAL    ETR,ETAR,ETA_DETR,PHIR
      REAL    ETD,ETAD,ETA_DETD,PHID
      REAL    ETC,ETAC,ETA_DETC,PHIC
      REAL    EL_REF,EM_REF,EN_REF,EL_DTA,EM_DTA,EN_DTA
      REAL    DPT,DETA,DPHI,COSP,DCOS_FROM_ETA_PHI
      REAL    PT_MIN,PT_MAX
      REAL    HCONT(5,50),HCERR(5,50),DUMMY(50),DUMME(50),A,B
      REAL    RVAL(6)
      LOGICAL FIRST,ESUM_REJEFF, EVENT_LIST
      LOGICAL ID( ID_ALL : LAST_TYPE )
      LOGICAL PT_SPEC,LOC,RES,WRUN,REF_MATCH
      LOGICAL MRF(40),MDT(40),PRF(40),PDT(40),PRQ(40),SRQ(40)
      LOGICAL LREQ, LMATCH_REF(40), LMATCH_DTA(40)
      LOGICAL CMATCH_REQ(7),LVAL(7),FLAG_SET
      EXTERNAL RUNNO, EVONUM
      EQUIVALENCE (IVAL(1),RVAL(1))
      EQUIVALENCE (IVAL(1),LVAL(1))
      SAVE FIRST
      DATA FIRST/.TRUE./
      PARAMETER( NPAIRS = 50 )
      PARAMETER( NMX = 40 )
C----------------------------------------------------------------------
C
C ****  Initialize
C
      ESUM_COMPARE = .FALSE.
      CALL FLGSET('WRITE_STREAM_DST',.FALSE.)
      IF( FIRST ) THEN
        FIRST = .FALSE.
        CALL ESUM_INIT( OB_RESOLVE, OB_THRESH, OB_NAME )
C
C ****   Read RCP file and book Histograms
C
        CALL INRCP('ESUM_COMPARE_RCP',IER_SUB)
        IF ( IER_SUB.NE.0 ) THEN
          WRITE(MSG,'(A,I5)')'IER from INRCP = ',IER_SUB
          CALL ERRMSG('INRCP_IER','ESUM_COMPARE',MSG,'W')
          GO TO 999
        ENDIF
        CALL EZPICK('ESUM_COMPARE_RCP')
        CALL EZGETS('REF',1,REF,L,IER_SUB)
        CALL EZGETS('DTA',1,DTA,L,IER_SUB)
        CALL EZGETS('REQ',1,REQ,L,IER_SUB)
        CALL EZGET('CSP',CSP,IER_SUB)
        CALL EZGET_l('PT_SPEC',PT_SPEC,IER_SUB)
        CALL EZGET('PT_MIN',PT_MIN,IER_SUB)
        CALL EZGET('PT_MAX',PT_MAX,IER_SUB)
        CALL EZGET_l('LOC',LOC,IER_SUB)
        CALL EZGET_l('RES',RES,IER_SUB)
        CALL EZGET_l('REF_MATCH',REF_MATCH,IER_SUB)
        CALL EZGET_l('EVENT_LIST',EVENT_LIST,IER_SUB)
        CALL EZGETS('EVENT_LIST_FILENAME',1,EVLST_FILE,L,IER_SUB)
        IF (EVENT_LIST) THEN
          CALL GTUNIT(4691,WUNIT,IER)
          CALL D0OPEN(WUNIT,EVLST_FILE,'OF',IER)
          WRITE(WUNIT,55)
   55     FORMAT('   RUN NUMBER       EVENT#   FLAGS')
          CALL GTUNIT(4692,PUNIT,IER)
          STATUS = TRNLNM(EVLST_FILE,EVENTS_FILE,NCHARS)
          I = INDEX(EVENTS_FILE,']')
          EVLST_FILE = EVENTS_FILE(1:I)//'PICK_EV-'//
     &      EVENTS_FILE(I+1:NCHARS)
          CALL D0OPEN(PUNIT,EVLST_FILE,'OF',IER)
        END IF
        RUNO = RUNNO()
        NEVT = 0
        DO I = 1,100
          EVENT(I) = 0
        ENDDO
        DTHETA = ACOS(CSP)
        PHI_BINS = 63
        ETA_BINS = 40
        NID = 1
        IDPAR = 1
        CALL EZGNXT('PAIR',NID,IDPAR)
        IF ( IDPAR.EQ.0 ) THEN
          WRITE(MSG,'(A)')'No PAIRs requested, Check RCP file'
          CALL ERRMSG('ESUM_COMPARE_IER','ESUM_COMPARE',MSG,'W')
          GO TO 999
        ELSE
          DO WHILE ( IDPAR.NE.0 )
            CALL EZGETN(IDPAR,ARRAY_NAME,N)
            CALL EZGET_VALUE_TYPE(ARRAY_NAME,IVAL,TYPE,NVAL,IER_SUB)
            ID_OBJ_REF(NID-1) = IVAL(1)
            ID_OBJ_DTA(NID-1) = IVAL(3)
            ID_OBJ_REQ(NID-1) = IVAL(5)
            ETTHR_REF(NID-1)  = RVAL(2)
            ETTHR_DTA(NID-1)  = RVAL(4)
            ETTHR_REQ(NID-1)  = RVAL(6)
            CMATCH_REQ(NID-1) = LVAL(7)
            CALL EZGNXT('PAIR',NID,IDPAR)
          ENDDO
          NUM_PAIRS = NID-2
        ENDIF
        IF ( NUM_PAIRS.GT.NPAIRS ) THEN
          WRITE(MSG,'(A,I3)')'No. of PAIRs requested is > ',NPAIRS
          CALL ERRMSG('ESUM_COMPARE_IER','ESUM_COMPARE',MSG,'W')
          GO TO 999
        ENDIF
        DO I = 1,NUM_PAIRS
          IDHN = 10
          IREF = ID_OBJ_REF(I)
          IDTA = ID_OBJ_DTA(I)
          IREQ = ID_OBJ_REQ(I)
          J = 3
          K = 3
          CALL WORD(OB_NAME(IREF),ISTR(I),IENR(I),LNGR)
          CALL WORD(OB_NAME(IDTA),ISTD(I),IEND(I),LNGD)
          IF ((IENR(I) - ISTR(I)).LT.J) J = IENR(I) - ISTR(I)
          IF ((IEND(I) - ISTD(I)).LT.K) K = IEND(I) - ISTD(I)
          HBOOK_DIR(I) = OB_NAME(IREF)(ISTR(I):ISTR(I)+J)//'-'//
     &       OB_NAME(IDTA)(ISTD(I):ISTD(I)+K)
          IF (REQ.NE.' ' .AND. IREQ.NE.99) THEN
            L = 3
            CM = '  '
            CALL WORD(HBOOK_DIR(I),J,K,LEN)
            CALL WORD(OB_NAME(IREQ),ISTC(I),IENC(I),LNGC)
            IF ((IENC(I) - ISTC(I)).LT.L) L = IENC(I) - ISTC(I)
            IF (CMATCH_REQ(I)) CM = '-M'
            HBOOK_DIR(I) = HBOOK_DIR(I)(J:K)//'-'//
     &        OB_NAME(IREQ)(ISTC(I):ISTC(I)+L)//CM
          ENDIF
          CALL HMDIR(HBOOK_DIR(I),'S')
          IF ( PT_SPEC ) THEN
            IDH = IDHN+1
            TITLE = REF//'-'//OB_NAME(IREF)(ISTR(I):IENR(I))//
     &        ' Pt-distribution'
            CALL HBOOK1(IDH,TITLE,50,PT_MIN,PT_MAX,0.)
            IDH = IDHN+18
            TITLE =
     &          'Integral Eff. ('//DTA//'(MATCH)-'//OB_NAME(IDTA)
     &          (ISTD(I):IEND(I))//'s/'//REF//'(TOTAL)-'//OB_NAME(IREF)
     &          (ISTR(I):IENR(I))//'s) .vs. Pt of '
     &          //REF//'-'//OB_NAME(IREF)(ISTR(I):IENR(I))//'s'
            CALL HBOOK1(IDH,TITLE,50,PT_MIN,PT_MAX,0.)
            IDH = IDHN+2
            TITLE = DTA//'-'//OB_NAME(IDTA)(ISTD(I):IEND(I))//
     &        ' Pt-distribution'
            CALL HBOOK1(IDH,TITLE,50,PT_MIN,PT_MAX,0.)
            IDH = IDHN+3
            TITLE = 'MATCHed-'//REF//'-'//OB_NAME(IREF)(ISTR(I):IENR(I))
     &          //' Pt-distribution'
            CALL HBOOK1(IDH,TITLE,50,PT_MIN,PT_MAX,0.)
            IDH = IDHN+4
            TITLE = 'MATCHed-'//DTA//'-'//OB_NAME(IDTA)(ISTD(I):IEND(I))
     &          //' Pt-distribution'
            CALL HBOOK1(IDH,TITLE,50,PT_MIN,PT_MAX,0.)
            IDH = IDHN+33
            TITLE='UNMATCHed-'//REF//'-'//OB_NAME(IREF)(ISTR(I):IENR(I))
     &          //' Pt-distribution'
            CALL HBOOK1(IDH,TITLE,50,PT_MIN,PT_MAX,0.)
            IDH = IDHN+34
            TITLE='UNMATCHed-'//DTA//'-'//OB_NAME(IDTA)(ISTD(I):IEND(I))
     &          //' Pt-distribution'
            CALL HBOOK1(IDH,TITLE,50,PT_MIN,PT_MAX,0.)
            IDH = IDHN+27
            TITLE =
     &          'Integral  ('//DTA//'(MATCH)-'//OB_NAME(IDTA)
     &          (ISTD(I):IEND(I))//'s/'//REF//'(MATCH)-'//OB_NAME(IREF)
     &          (ISTR(I):IENR(I))//'s) .vs. Pt of '
     &          //REF//'-'//OB_NAME(IREF)(ISTR(I):IENR(I))//'s'
            CALL HBOOK1(IDH,TITLE,50,PT_MIN,PT_MAX,0.)
            IF ( REF_MATCH ) THEN
              IDH = IDHN+29
              TITLE =
     &          'Integral ('//REF//'(MATCH)-'//OB_NAME(IDTA)
     &          (ISTD(I):IEND(I))//'s/'//DTA//'(TOTAL)-'//OB_NAME(IREF)
     &          (ISTR(I):IENR(I))//'s) .vs. Pt of '
     &          //REF//'-'//OB_NAME(IREF)(ISTR(I):IENR(I))//'s'
              CALL HBOOK1(IDH,TITLE,50,PT_MIN,PT_MAX,0.)
            ELSE
              IDH = IDHN+31
              TITLE =
     &          'Integral ('//DTA//'(MATCH)-'//OB_NAME(IDTA)
     &          (ISTD(I):IEND(I))//'s/'//DTA//'(TOTAL)-'//OB_NAME(IREF)
     &          (ISTR(I):IENR(I))//'s) .vs. Pt of '
     &          //REF//'-'//OB_NAME(IREF)(ISTR(I):IENR(I))//'s'
              CALL HBOOK1(IDH,TITLE,50,PT_MIN,PT_MAX,0.)
            ENDIF
          ENDIF
          IF ( LOC ) THEN
            IDH = IDHN+5
            TITLE = 'MATCHed-'//REF//'-'//OB_NAME(IREF)(ISTR(I):IENR(I))
     &          //' PHI-ETA Scatter Plot'
            CALL HBOOK2(IDH,TITLE,ETA_BINS,-4.,4.,PHI_BINS,0.,6.3,0.)
            IDH = IDHN+6
            TITLE = 'LOST-'//REF//'-'//OB_NAME(IREF)(ISTR(I):IENR(I))
     &          //' PHI-ETA Scatter Plot'
            CALL HBOOK2(IDH,TITLE,ETA_BINS,-4.,4.,PHI_BINS,0.,6.3,0.)
            IDH = IDHN+7
            TITLE = 'REJECTed-'//DTA//'-'//OB_NAME(IDTA)(ISTD(I):
     &        IEND(I))//' PHI-ETA Scatter Plot'
            CALL HBOOK2(IDH,TITLE,ETA_BINS,-4.,4.,PHI_BINS,0.,6.3,0.)
            IDH = IDHN+35
            TITLE = 'MATCHed-'//REF//'-'//OB_NAME(IREF)(ISTR(I):IENR(I))
     &          //' Pt-ETA Scatter Plot'
            CALL HBOOK2(IDH,TITLE,50,PT_MIN,PT_MAX,ETA_BINS,-4.,4.,0.)
            IDH = IDHN+36
            TITLE = 'LOST-'//REF//'-'//OB_NAME(IREF)(ISTR(I):IENR(I))
     &          //' Pt-ETA Scatter Plot'
            CALL HBOOK2(IDH,TITLE,50,PT_MIN,PT_MAX,ETA_BINS,-4.,4.,0.)
            IDH = IDHN+37
            TITLE = 'REJECTed-'//DTA//'-'//OB_NAME(IDTA)(ISTD(I):
     &        IEND(I))//' Pt-ETA Scatter Plot'
            CALL HBOOK2(IDH,TITLE,50,PT_MIN,PT_MAX,ETA_BINS,-4.,4.,0.)
          ENDIF
          IF ( RES ) THEN
            IDH = IDHN+8
            TITLE = 'MATCHed-'//REF//'-'//OB_NAME(IREF)(ISTR(I):IENR(I))
     &          //' (dPt/Pt)-Pt Scatter Plot'
            CALL HBOOK2(IDH,TITLE,50,PT_MIN,PT_MAX,50,-2.,2.,0.)
            IDH = IDHN+9
            TITLE = 'MATCHed-'//REF//'-'//OB_NAME(IREF)(ISTR(I):IENR(I))
     &          //' d(ETA)-ETA Scatter Plot'
            CALL HBOOK2(IDH,TITLE,50,-4.,4.,50,-DTHETA,DTHETA,0.)
            IDH = IDHN+10
            TITLE = 'MATCHed-'//REF//'-'//OB_NAME(IREF)(ISTR(I):IENR(I))
     &          //' d(PHI)-PHI Scatter Plot'
            CALL HBOOK2(IDH,TITLE,PHI_BINS,0.,6.3,50,-DTHETA,DTHETA,0.)
            IDH = IDHN+11
            TITLE = 'MATCHed-'//REF//'-'//OB_NAME(IREF)(ISTR(I):IENR(I))
     &          //' d(Cos(Space angle)) .vs. Pt Scatter Plot'
            CALL HBOOK2(IDH,TITLE,50,PT_MIN,PT_MAX,100,CSP,1.,0.)
            IDH = IDHN+12
            TITLE = 'MATCHed-'//REF//'-'//OB_NAME(IREF)(ISTR(I):IENR(I))
     &          //' (dPt/Pt)-Pt Profile Plot'
            CALL HBPROF(IDH,TITLE,50,PT_MIN,PT_MAX,-2.,2.,'S')
            IDH = IDHN+13
            TITLE = 'MATCHed-'//REF//'-'//OB_NAME(IREF)(ISTR(I):IENR(I))
     &          //' d(ETA)-ETA Profile Plot'
            CALL HBPROF(IDH,TITLE,ETA_BINS,-4.,4.,-DTHETA,DTHETA,'S')
            IDH = IDHN+14
            TITLE = 'MATCHed-'//REF//'-'//OB_NAME(IREF)(ISTR(I):IENR(I))
     &          //' d(PHI)-PHI Profile Plot'
            CALL HBPROF(IDH,TITLE,PHI_BINS,0.,6.3,-DTHETA,DTHETA,'S')
            IDH = IDHN+15
            TITLE = 'MATCHed-'//REF//'-'//OB_NAME(IREF)(ISTR(I):IENR(I))
     &          //' d(Cos(Space angle))-Pt Profile Plot'
            CALL HBPROF(IDH,TITLE,50,PT_MIN,PT_MAX,CSP,1.,'S')
            IDH = IDHN+16
            TITLE = 'Pt-Correlation of MATCHed '//DTA//'-'//
     &          OB_NAME(IDTA)(ISTD(I):IEND(I))//'s and '//REF//
     &          '-'//OB_NAME(IREF)(ISTR(I):IENR(I))//'s'
            CALL HBOOK2(IDH,TITLE,50,PT_MIN,PT_MAX,50,PT_MIN,PT_MAX,
     &          0.)
            IDH = IDHN+17
            TITLE = 'd(Pt)/Pt .vs. d(Cos(Space angle'//')) for MATCHed '
     &        //DTA//'-'//OB_NAME(IDTA)(ISTD(I):IEND(I))//'s and '
     &        //REF//'-'//OB_NAME(IREF)(ISTR(I):IENR(I))//'s'
            CALL HBOOK2(IDH,TITLE,100,CSP,1.,50,-2.,2.,0.)
          ENDIF
          IDH = IDHN+20
          TITLE = 'd(Pt)/Pt distribution for MATCHed '
     &        //DTA//'-'//OB_NAME(IDTA)(ISTD(I):IEND(I))//'s and '
     &        //REF//'-'//OB_NAME(IREF)(ISTR(I):IENR(I))//'s'
          CALL HBOOK1(IDH,TITLE,50,-2.,2.,0.)
          IDH = IDHN+21
          TITLE = 'd(Eta) distribution for MATCHed '
     &        //DTA//'-'//OB_NAME(IDTA)(ISTD(I):IEND(I))//'s and '
     &        //REF//'-'//OB_NAME(IREF)(ISTR(I):IENR(I))//'s'
          CALL HBOOK1(IDH,TITLE,50,-DTHETA,DTHETA,0.)
          IDH = IDHN+22
          TITLE = 'd(Phi) distribution for MATCHed '
     &        //DTA//'-'//OB_NAME(IDTA)(ISTD(I):IEND(I))//'s and '
     &        //REF//'-'//OB_NAME(IREF)(ISTR(I):IENR(I))//'s'
          CALL HBOOK1(IDH,TITLE,50,-DTHETA,DTHETA,0.)
          IDH = IDHN+23
          TITLE = REF//'-'//OB_NAME(IREF)(ISTR(I):IENR(I))//
     &        ' Eta-distribution'
          CALL HBOOK1(IDH,TITLE,ETA_BINS,-4.,4.,0.)
C          CALL HBARX (IDH)
          IDH = IDHN+24
          TITLE = DTA//'-'//OB_NAME(IDTA)(ISTR(I):IENR(I))//
     &        ' Eta-distribution'
          CALL HBOOK1(IDH,TITLE,ETA_BINS,-4.,4.,0.)
C          CALL HBARX (IDH)
          IDH = IDHN+25
          TITLE = 'MATCHed-'//REF//'-'//OB_NAME(IREF)(ISTR(I):IENR(I))
     &          //' Eta-distribution'
          CALL HBOOK1(IDH,TITLE,ETA_BINS,-4.,4.,0.)
C          CALL HBARX (IDH)
          IDH = IDHN+26
          TITLE = 'MATCHed-'//DTA//'-'//OB_NAME(IDTA)(ISTR(I):IENR(I))
     &          //' Eta-distribution'
          CALL HBOOK1(IDH,TITLE,ETA_BINS,-4.,4.,0.)
C          CALL HBARX (IDH)
          IDH = IDHN+28
          TITLE =
     &          '('//DTA//'(MATCH)-'//OB_NAME(IDTA)
     &          (ISTD(I):IEND(I))//'s/'//REF//'(MATCH)-'//OB_NAME(IREF)
     &          (ISTR(I):IENR(I))//'s) .vs. Eta of '
     &          //REF//'-'//OB_NAME(IREF)(ISTR(I):IENR(I))//'s'
          CALL HBOOK1(IDH,TITLE,ETA_BINS,-4.,4.,0.)
C          CALL HBARX (IDH)
          IF ( REF_MATCH ) THEN
            IDH = IDHN+30
            TITLE =
     &          '('//REF//'(MATCH)-'//OB_NAME(IDTA)
     &          (ISTD(I):IEND(I))//'s/'//DTA//'(TOTAL)-'//OB_NAME(IREF)
     &          (ISTR(I):IENR(I))//'s) .vs. Eta of '
     &          //REF//'-'//OB_NAME(IREF)(ISTR(I):IENR(I))//'s'
            CALL HBOOK1(IDH,TITLE,ETA_BINS,-4.,4.,0.)
C            CALL HBARX (IDH)
          ELSE
            IDH = IDHN+32
            TITLE =
     &          '('//DTA//'(MATCH)-'//OB_NAME(IDTA)
     &          (ISTD(I):IEND(I))//'s/'//DTA//'(TOTAL)-'//OB_NAME(IREF)
     &          (ISTR(I):IENR(I))//'s) .vs. Eta of '
     &          //REF//'-'//OB_NAME(IREF)(ISTR(I):IENR(I))//'s'
            CALL HBOOK1(IDH,TITLE,ETA_BINS,-4.,4.,0.)
C            CALL HBARX (IDH)
          ENDIF
          IDH = 5001
          TITLE = 'Dummy 1'
          CALL HBOOK1 (IDH,TITLE,50,PT_MIN,PT_MAX,0.)
          IDH = 5002
          TITLE = 'Dummy 2'
          CALL HBOOK1 (IDH,TITLE,50,PT_MIN,PT_MAX,0.)
          IDH = 5003
          TITLE = 'Dummy 3'
          CALL HBOOK1 (IDH,TITLE,50,PT_MIN,PT_MAX,0.)
          IDH = 5004
          TITLE = 'Dummy 4'
          CALL HBOOK1 (IDH,TITLE,50,PT_MIN,PT_MAX,0.)
C&IF IBMAIX
C&          CALL HCDIR('\\',' ')
C&ELSE
          CALL HCDIR('\',' ')
C&ENDIF
        ENDDO
        CALL HIDOPT (0,'STAT')
        CALL EZRSET
      ENDIF
      ESUM_COMPARE = .TRUE.
      IF(REF.EQ.'ISAE'.OR.DTA.EQ.'ISAE') CALL ISAE_ESUM
C
C ****  Write out the event list before processing the next run
C
      RUN   = RUNNO()
      IF (EVENT_LIST) THEN
        IF (RUN.NE.RUNO.AND.NEVT.NE.0) THEN
          WRITE(PUNIT,550) RUNO, NEVT
          DO I=1,NEVT
            WRITE(PUNIT,551) EVENT(I)
          END DO
          RUNO = RUN
          NEVT = 0
  550     FORMAT(3X,I10,3X,I5)
  551     FORMAT(6X,I10)
        END IF
      END IF

C
C ****  Fetch # of objects from REF and DTA banks
C
      CALL GTESUM_COUNTS (REF,NFOUND_REF,IER_SUB)
      IF ( IER_SUB.NE.0 ) THEN          ! no ESUM bank of REF type
        WRITE(MSG,'(A,I5)')'IER from GTESUM_COUNTS = ',IER_SUB
        CALL ERRMSG('GTESUM_COUNTS_IER','ESUM_COMPARE',MSG,'W')
        GO TO 999
      ENDIF
      CALL GTESUM_COUNTS (DTA,NFOUND_DTA,IER_SUB)
      IF ( IER_SUB.NE.0 ) THEN          ! no ESUM bank of DTA type
        WRITE(MSG,'(A,I5)')'IER from GTESUM_COUNTS = ',IER_SUB
        CALL ERRMSG('GTESUM_COUNTS_IER','ESUM_COMPARE',MSG,'W')
        GO TO 999
      ENDIF
      IF (REQ.NE.' ') THEN
        CALL GTESUM_COUNTS (REQ,NFOUND_REQ,IER_SUB)
        IF ( IER_SUB.NE.0 ) THEN          ! no ESUM bank of REQ type
          WRITE(MSG,'(A,I5)')'IER from GTESUM_COUNTS = ',IER_SUB
          CALL ERRMSG('GTESUM_COUNTS_IER','ESUM_COMPARE',MSG,'W')
          GO TO 999
        ENDIF
      ENDIF
      FLAG_SET = .FALSE.
C
C ****  Loop over requested pairs of objects
C
      DO I = 1,NUM_PAIRS
        IDHN = 10
C
C ****  Look for MATCHES
C
        IDO(1) = ID_OBJ_REF(I)
        IDO(2) = ID_OBJ_DTA(I)
        CALL GTESUM_MATCH (REF,DTA,IDO,CSP,NMX,MRF,MDT,NMC,MTM,IER)
        IF(IER.NE.0) THEN
          WRITE(MSG,'(A,I5)')'IER from GTESUM_MATCH = ',IER
          CALL ERRMSG('GTESUM_MATCH_IER','ESUM_COMPARE',MSG,'W')
        ENDIF
C
C ****  Setup arrays for match with REQ object
C
        LREQ = .FALSE.
        IF (REQ.NE.' ' .AND. ID_OBJ_REQ(I).NE.99) THEN

          LREQ = .TRUE.
          DO J=1,40
            LMATCH_REF(J) = .FALSE.
            LMATCH_DTA(J) = .FALSE.
          END DO

          IDO(1) = ID_OBJ_REF(I)
          IDO(2) = ID_OBJ_REQ(I)

          CALL GTESUM_MATCH (REF,REQ,IDO,CSP,NMX,PRF,PRQ,PMC,PMAT,IER)
          IF(IER.NE.0) THEN
            WRITE(MSG,'(A,I5)')'IER from GTESUM_MATCH = ',IER
            CALL ERRMSG('GTESUM_MATCH_IER','ESUM_COMPARE',MSG,'W')
          ENDIF
          DO J = 1, PMC
            CALL GTESUM (REQ,IDO(2),PMAT(J,2),ETC,ETAC,ETA_DETC,PHIC,
     &              FLAGC,IER_SUB)
            IF (ETC.LE.ETTHR_REQ(I)) THEN
              PRF(PMAT(J,1)) = .FALSE.
              PRQ(PMAT(J,2)) = .FALSE.
            END IF
          END DO

          IDO(1) = ID_OBJ_DTA(I)
          IDO(2) = ID_OBJ_REQ(I)

          CALL GTESUM_MATCH (DTA,REQ,IDO,CSP,NMX,PDT,SRQ,SMC,SMAT,IER)
          IF(IER.NE.0) THEN
            WRITE(MSG,'(A,I5)')'IER from GTESUM_MATCH = ',IER
            CALL ERRMSG('GTESUM_MATCH_IER','ESUM_COMPARE',MSG,'W')
          ENDIF
          DO J = 1, SMC
            CALL GTESUM (REQ,IDO(2),SMAT(J,2),ETC,ETAC,ETA_DETC,PHIC,
     &              FLAGC,IER_SUB)
            IF (ETC.LE.ETTHR_REQ(I)) THEN
              PDT(SMAT(J,1)) = .FALSE.
              SRQ(SMAT(J,2)) = .FALSE.
            END IF
            IF (CMATCH_REQ(I)) THEN
              DO K=1,NMC
                IF (PMAT(K,2).EQ.SMAT(J,2)) THEN
                  LMATCH_REF(PMAT(K,1)) = .TRUE.
                  LMATCH_DTA(SMAT(J,1)) = .TRUE.
                END IF
              END DO
            END IF
          END DO

          IDO(1) = ID_OBJ_REF(I)
          IDO(2) = ID_OBJ_DTA(I)
          IDO3 = ID_OBJ_REQ(I)

          IF (CMATCH_REQ(I)) THEN
            DO J = 1, NFOUND_REF(IDO(1))
              IF (PRF(J) .AND. (.NOT.LMATCH_REF(J)))
     &          PRF (J) = LMATCH_REF(J)
            END DO
            DO J = 1, NFOUND_DTA(IDO(2))
              IF (PDT(J) .AND. (.NOT.LMATCH_DTA(J)))
     &          PDT (J) = LMATCH_DTA(J)
            END DO
          END IF

        END IF
C
C ****  set HBOOK directory
C
        CHDIR = '//PAWC/'//HBOOK_DIR(I)
        CALL HCDIR(CHDIR,' ')     ! change HBOOK directory
        EV_FLAG(I) = 0
C
C ****  Process all REF objects
C
        IF(NFOUND_REF(IDO(1)).NE.0) THEN
          DO J = 1,NFOUND_REF(IDO(1))
            IF (LREQ) THEN
              IF ( (NFOUND_REQ(IDO3).EQ.0) .OR. (.NOT.PRF(J)) )
     &          GOTO 200
            END IF
            CALL GTESUM (REF,IDO(1),J,ET,ETA,ETA_DET,PHI,FLAG,IER_SUB)
            IF (ET.GT.ETTHR_REF(I)) THEN
              IDH = IDHN+1
              IF (PT_SPEC) CALL HFILL (IDH,ET,0.,1.)
              IDH = IDHN+23
              CALL HFILL (IDH,ETA_DET,0.,1.)

C
C ****  Process MATCHed REF objects
C
              IF ( MRF(J) ) THEN
                IF (PT_SPEC) THEN
                  IDH = IDHN+3
                  CALL HFILL (IDH,ET,0.,1.)
                  IDH = IDHN+35
                  CALL HFILL (IDH,ET,ETA_DET,1.)
                END IF
                IDH = IDHN+5
                IF (LOC) CALL HFILL (IDH,ETA_DET,PHI,1.)
                IDH = IDHN+25
                CALL HFILL (IDH,ETA_DET,0.,1.)
              ELSE
C
C ****  Process UNMATCHed REF objects
C
                IDH = IDHN+6
                IF (LOC) CALL HFILL (IDH,ETA_DET,PHI,1.)
                IF (PT_SPEC) THEN
                  IDH = IDHN+33
                  CALL HFILL (IDH,ET,0.,1.)
                  CALL FLGSET('WRITE_STREAM_DST',.TRUE.)
                  IF (EVENT_LIST) THEN
                    EV_FLAG(I) = EV_FLAG(I) + 1
                    FLAG_SET   = .TRUE.
                  END IF
                  IDH = IDHN+36
                  CALL HFILL (IDH,ET,ETA_DET,1.)
                END IF
              ENDIF
            ENDIF
  200       CONTINUE
          ENDDO
        ENDIF
C
C ****  Process all DTA objects
C
        IF(NFOUND_DTA(IDO(2)).NE.0) THEN
          DO J = 1 ,NFOUND_DTA(IDO(2))
            IF (LREQ) THEN
              IF ( (NFOUND_REQ(IDO3).EQ.0) .OR. (.NOT.PDT(J)) )
     &          GOTO 300
            END IF
            CALL GTESUM (DTA,IDO(2),J,ET,ETA,ETA_DET,PHI,FLAG,IER_SUB)
            IF (ET.GT.ETTHR_DTA(I)) THEN
              IDH = IDHN+2
              IF (PT_SPEC) CALL HFILL (IDH,ET,0.,1.)
              IDH = IDHN+24
              CALL HFILL (IDH,ETA_DET,0.,1.)

C
C ****  Process MATCHed DTA objects
C
              IF ( MDT(J) ) THEN
                IDH = IDHN+4
                IF (PT_SPEC) CALL HFILL (IDH,ET,0.,1.)
                IDH = IDHN+26
                CALL HFILL (IDH,ETA_DET,0.,1.)
              ELSE
C
C ****  Process REJECTed DTA objects
C
                IDH = IDHN+34
                IF (PT_SPEC) THEN
                  CALL HFILL (IDH,ET,0.,1.)
                  CALL FLGSET('WRITE_STREAM_DST',.TRUE.)
                  IF (EVENT_LIST) THEN
                    EV_FLAG(I) = EV_FLAG(I) + 2
                    FLAG_SET   = .TRUE.
                  END IF
                END IF
                IF (LOC) THEN
                  IDH = IDHN+7
                  CALL HFILL (IDH,ETA_DET,PHI,1.)
                  IDH = IDHN+37
                  CALL HFILL (IDH,ET,ETA_DET,1.)
                END IF
              ENDIF
            ENDIF
  300       CONTINUE
          ENDDO
        ENDIF
C
C ****  Process MATCHed pairs
C
        IF(NFOUND_REF(IDO(1)).NE.0.AND.NFOUND_DTA(IDO(2)).NE.0) THEN
          DO J = 1 ,NMC
            CALL GTESUM (REF,IDO(1),MTM(J,1),ETR,ETAR,ETA_DETR,PHIR,
     &            FLAGR,IER_SUB)
            IF (ETR.GT.ETTHR_REF(I)) THEN
              CALL GTESUM (DTA,IDO(2),MTM(J,2),ETD,ETAD,ETA_DETD,PHID,
     &              FLAGD,IER_SUB)
              IF (ETD.GT.ETTHR_DTA(I)) THEN
                DPT = (ETD-ETR)/ETR
                DETA = ETA_DETD-ETA_DETR
                DPHI = PHID-PHIR
                IDH = IDHN+20
                CALL HFILL (IDH,DPT,0.,1.)
                IDH = IDHN+21
                CALL HFILL (IDH,DETA,0.,1.)
                IDH = IDHN+22
                CALL HFILL (IDH,DPHI,0.,1.)
                IF ( RES ) THEN
                  IDH = IDHN+8
                  CALL HFILL (IDH,ETR,DPT,1.)
                  IDH = IDHN+12
                  CALL HFILL (IDH,ETR,DPT,1.)
                  IDH = IDHN+9
                  CALL HFILL (IDH,ETA_DETR,DETA,1.)
                  IDH = IDHN+13
                  CALL HFILL (IDH,ETA_DETR,DETA,1.)
                  IDH = IDHN+10
                  CALL HFILL (IDH,PHIR,DPHI,1.)
                  IDH = IDHN+14
                  CALL HFILL (IDH,PHIR,DPHI,1.)
                  COSP = DCOS_FROM_ETA_PHI(ETA_DETR,PHIR,ETA_DETD,PHID)
                  IDH = IDHN+11
                  CALL HFILL (IDH,ETR,COSP,1.)
                  IDH = IDHN+15
                  CALL HFILL (IDH,ETR,COSP,1.)
                  IDH = IDHN+16
                  CALL HFILL (IDH,ETR,ETD,1.)
                  IDH = IDHN+17
                  CALL HFILL (IDH,COSP,DPT,1.)
                ENDIF
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDDO

      IF (EVENT_LIST .AND. FLAG_SET) THEN
        NEVT = NEVT + 1
        EVENT(NEVT) = EVONUM()
C&IF IBMAIX, LINUX    ! ibm and g77 don't have variable formats
C&        WRITE (msg, 555) num_pairs
C& 555    format ('(2(3x,i10),', i3, '(2x,i4))')
C&        write (wunit, msg) RUN,EVENT(NEVT),(EV_FLAG(I),I=1,NUM_PAIRS)
C&ELSE
        WRITE(WUNIT,555) RUN,EVENT(NEVT),(EV_FLAG(I),I=1,NUM_PAIRS)
  555   FORMAT(2(3X,I10),<NUM_PAIRS>(2X,I4))
C&ENDIF
      END IF

  999 RETURN
C
      ENTRY ESUM_REJEFF
C
      ESUM_REJEFF = .TRUE.
C
C ****  Form Efficiency and Rejection Histograms
C
      DO I = 1,NUM_PAIRS
        CHDIR = '//PAWC/'//HBOOK_DIR(I)
        CALL HCDIR(CHDIR,' ')
        IF ( PT_SPEC ) THEN
          IDHN = 10
          DO J = 1,4
            IDH = IDHN+J
            CALL HUNPAK (IDH,DUMMY,' ',0)
            DO K = 49,1,-1
              DUMMY(K) = DUMMY(K)+DUMMY(K+1)
              HCONT(J,K) = DUMMY(K)
            ENDDO
            HCONT(J,50) = DUMMY(50)
            DO K = 1,50
              HCERR(J,K) = SQRT(HCONT(J,K))
              DUMME(K) = HCERR(J,K)
            ENDDO
            IDH = 5000+J
            CALL HPAK (IDH,DUMMY)
            CALL HPAKE (IDH,DUMME)
          ENDDO
          ID1 = 5004
          ID2 = 5001
          ID3 = IDHN+18
          CALL HOPERA (ID1,'/',ID2,ID3,1.,1.) ! (MATCH_DTA/TOTAL_REF)
          CALL HUNPAK (ID3,DUMMY,' ',0)
          DO K = 1,50
            IF ( HCONT(4,K).EQ.0 ) THEN
              A = 0.
            ELSE
              A = 1./HCONT(4,K)
            ENDIF
            IF ( HCONT(1,K).EQ.0 ) THEN
              B = 0.
            ELSE
              B = 1./HCONT(1,K)
            ENDIF
            DUMME(K) = DUMMY(K)*SQRT(A+B)
          ENDDO
          CALL HPAKE (ID3,DUMME)
          ID1 = 5004
          ID2 = 5003
          ID3 = IDHN+27
          CALL HOPERA (ID1,'/',ID2,ID3,1.,1.)    ! (MATCH_DTA/MATCH_REF) vs
                                                 ! Pt
          CALL HUNPAK (ID3,DUMMY,' ',0)
          DO K = 1,50
            IF ( HCONT(4,K).EQ.0 ) THEN
              A = 0.
            ELSE
              A = 1./HCONT(4,K)
            ENDIF
            IF ( HCONT(3,K).EQ.0 ) THEN
              B = 0.
            ELSE
              B = 1./HCONT(3,K)
            ENDIF
            DUMME(K) = DUMMY(K)*SQRT(A+B)
          ENDDO
          CALL HPAKE (ID3,DUMME)
          IF ( REF_MATCH ) THEN
            ID1 = 5003
            ID2 = 5002
            ID3 = IDHN+29
            CALL HOPERA (ID1,'/',ID2,ID3,1.,1.)    ! (MATCH_REF/TOTAL_DTA) vs
                                                   ! Pt
            CALL HUNPAK (ID3,DUMMY,' ',0)
            DO K = 1,50
              IF ( HCONT(2,K).EQ.0 ) THEN
                A = 0.
              ELSE
                A = 1./HCONT(2,K)
              ENDIF
              IF ( HCONT(3,K).EQ.0 ) THEN
                B = 0.
              ELSE
                B = 1./HCONT(3,K)
              ENDIF
              DUMME(K) = DUMMY(K)*SQRT(A+B)
            ENDDO
            CALL HPAKE (ID3,DUMME)
          ELSE
            ID1 = 5004
            ID2 = 5002
            ID3 = IDHN+31
            CALL HOPERA (ID1,'/',ID2,ID3,1.,1.)    ! (MATCH_DTA/TOTAL_DTA) vs
                                                   ! Pt
            CALL HUNPAK (ID3,DUMMY,' ',0)
            DO K = 1,50
              IF ( HCONT(2,K).EQ.0 ) THEN
                A = 0.
              ELSE
                A = 1./HCONT(2,K)
              ENDIF
              IF ( HCONT(4,K).EQ.0 ) THEN
                B = 0.
              ELSE
                B = 1./HCONT(4,K)
              ENDIF
              DUMME(K) = DUMMY(K)*SQRT(A+B)
            ENDDO
            CALL HPAKE (ID3,DUMME)
          ENDIF
        ENDIF
        ID1 = IDHN+26
        ID2 = IDHN+25
        ID3 = IDHN+28
        CALL HOPERA (ID1,'/',ID2,ID3,1.,1.)    ! (MATCH_DTA/MATCH_REF) vs
                                               ! Eta
        CALL HIDOPT (ID3,'ERRO')
        IF ( REF_MATCH ) THEN
          ID1 = IDHN+25
          ID2 = IDHN+24
          ID3 = IDHN+30
          CALL HOPERA (ID1,'/',ID2,ID3,1.,1.)    ! (MATCH_REF/TOTAL_DTA) vs
                                                 ! Eta
          CALL HIDOPT (ID3,'ERRO')
        ELSE
          ID1 = IDHN+26
          ID2 = IDHN+24
          ID3 = IDHN+32
          CALL HOPERA (ID1,'/',ID2,ID3,1.,1.)    ! (MATCH_DTA/TOTAL_DTA) vs
                                                 ! Eta
          CALL HIDOPT (ID3,'ERRO')
        ENDIF
        CALL HDELET (5001)
        CALL HDELET (5002)
        CALL HDELET (5003)
        CALL HDELET (5004)
      ENDDO
      RETURN
      END

