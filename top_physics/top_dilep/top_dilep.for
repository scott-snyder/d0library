      LOGICAL FUNCTION TOP_DILEP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TOP--> DILEPTON ANALYSIS
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  27-FEB-1994   Meenakshi Narain
C-   Updated  17-JAN-1995   Meenakshi Narain  
C-            Merge Jeff Bantly's request of 3 more groups MUISA, L2MUON, L2JET
C-                  and Ray Hall's request for a Z_FIT group
C-            Split of TRIGGER/FILTER information from GLB to its own
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:QUEST.INC'
      INTEGER NGROUPS
      PARAMETER( NGROUPS = 13 )
      CHARACTER*80 TOP_DIRNAME,NTFILE,TITLE
      CHARACTER*8  CHBLOK,GROUP_NAME(NGROUPS),CMAX
      CHARACTER*256 CHFORM
      CHARACTER*32  XNAMES(100),NTUP_NAMES(1000)
      REAL    XDATA(1000),NTUP_VAR(5000)
      INTEGER INTUP_VAR(5000)
      INTEGER NUM_TAGS(NGROUPS),NUM_REPS(NGROUPS)
      INTEGER IOFF_GENINFO,IOFF_ELEC,IOFF_MET,IOFF_JETS
      INTEGER IOFF_MUON,IOFF_MUISA,IOFF_GLBINFO,IOFF_PHOT,IOFF_BBISA
      INTEGER IOFF_L2MUON,IOFF_L2JET,IOFF_Z_FIT,IOFF_FILTINFO
      INTEGER NTAGS,NTVAR,NVAR,IER
      INTEGER I,J,K,L,IOFF,PTRFL,UNIT,NEXT_PTR
      INTEGER MAX_REC, LRECL, NWBUFF, ICYCLE
      INTEGER TRULEN,LEN,LEN_CHFORM,LUN,SSUNIT,LENC,MAX
      INTEGER NUM_INPUT, NUM_NTUPLE
      LOGICAL FIRST,TOP_DILEP_ANALYSIS,TOP_DILEP_FIN
      SAVE FIRST
      DATA FIRST / .TRUE. /
      EQUIVALENCE ( INTUP_VAR(1), NTUP_VAR(1) )
C----------------------------------------------------------------------
C-
C- Ntuple variables and definition.
C-
C----------------------------------------------------------------------
      IF( FIRST ) THEN
        FIRST = .FALSE.
        IOFF = 0
C
        CALL EVT_GENINFO_TAGS(NTAGS,XNAMES)
        IOFF_GENINFO = 0
        DO I=1, NTAGS
          NTUP_NAMES(I+IOFF) = XNAMES(I)
        ENDDO
        GROUP_NAME(1) = 'EVTINFO'
        NUM_TAGS(1) = NTAGS
        NUM_REPS(1) = 1
        IOFF = IOFF + NTAGS
C
        IOFF_MET = IOFF
        CALL MET_TAGS(NTAGS,XNAMES,MAX)
        CALL TOP_DILEP_SETUP_TAGS(NTAGS,XNAMES,NTUP_NAMES(1+IOFF))
        LENC = TRULEN(NTUP_NAMES(1+IOFF))
        WRITE(CMAX,'(''[0,'',I3.3,'']'')') MAX
        NTUP_NAMES(1+IOFF) = NTUP_NAMES(1+IOFF)(1:LENC)
     &    //CMAX(1:TRULEN(CMAX))//':I'
        GROUP_NAME(2) = 'MISSET'
        NUM_TAGS(2) = NTAGS
        NUM_REPS(2) = MAX
        IOFF = IOFF + NTAGS

C
        IOFF_ELEC = IOFF_MET + (NTAGS-1)*MAX + 1
        CALL ELEC_TAGS(NTAGS,XNAMES,MAX)
        CALL TOP_DILEP_SETUP_TAGS(NTAGS,XNAMES,NTUP_NAMES(1+IOFF))
        LENC = TRULEN(NTUP_NAMES(1+IOFF))
        WRITE(CMAX,'(''[0,'',I3.3,'']'')') MAX
        NTUP_NAMES(1+IOFF) = NTUP_NAMES(1+IOFF)(1:LENC)
     &    //CMAX(1:TRULEN(CMAX))//':I'
        GROUP_NAME(3) = 'ELEC'
        NUM_TAGS(3) = NTAGS
        NUM_REPS(3) = MAX
        IOFF = IOFF + NTAGS
C
        IOFF_PHOT = IOFF_ELEC + (NTAGS-1)*MAX + 1
        CALL PHOT_TAGS(NTAGS,XNAMES,MAX)
        CALL TOP_DILEP_SETUP_TAGS(NTAGS,XNAMES,NTUP_NAMES(1+IOFF))
        LENC = TRULEN(NTUP_NAMES(1+IOFF))
        WRITE(CMAX,'(''[0,'',I3.3,'']'')') MAX
        NTUP_NAMES(1+IOFF) = NTUP_NAMES(1+IOFF)(1:LENC)
     &    //CMAX(1:TRULEN(CMAX))//':I'
        GROUP_NAME(4) = 'PHOTON'
        NUM_TAGS(4) = NTAGS
        NUM_REPS(4) = MAX
        IOFF = IOFF + NTAGS
C
        IOFF_JETS = IOFF_PHOT + (NTAGS-1)*MAX + 1
        CALL JETS_TAGS(NTAGS,XNAMES,MAX)
        CALL TOP_DILEP_SETUP_TAGS(NTAGS,XNAMES,NTUP_NAMES(1+IOFF))
        LENC = TRULEN(NTUP_NAMES(1+IOFF))
        WRITE(CMAX,'(''[0,'',I3.3,'']'')') MAX
        NTUP_NAMES(1+IOFF) = NTUP_NAMES(1+IOFF)(1:LENC)
     &    //CMAX(1:TRULEN(CMAX))//':I'
        GROUP_NAME(5) = 'JETS_C05'
        NUM_TAGS(5) = NTAGS
        NUM_REPS(5) = MAX
        IOFF = IOFF + NTAGS
C
        IOFF_MUON = IOFF_JETS + (NTAGS-1)*MAX + 1
        CALL MUON_TAGS(NTAGS,XNAMES,MAX)
        CALL TOP_DILEP_SETUP_TAGS(NTAGS,XNAMES,NTUP_NAMES(1+IOFF))
        LENC = TRULEN(NTUP_NAMES(1+IOFF))
        WRITE(CMAX,'(''[0,'',I3.3,'']'')') MAX
        NTUP_NAMES(1+IOFF) = NTUP_NAMES(1+IOFF)(1:LENC)
     &    //CMAX(1:TRULEN(CMAX))//':I'
        GROUP_NAME(6) = 'ISOLMU'
        NUM_TAGS(6) = NTAGS
        NUM_REPS(6) = MAX
        IOFF = IOFF + NTAGS
C
        IOFF_GLBINFO = IOFF_MUON + (NTAGS-1)*MAX + 1
        CALL EVT_GLBINFO_TAGS(NTAGS,XNAMES)
        GROUP_NAME(7) = 'GLOBAL'
        DO I=1, NTAGS
          NTUP_NAMES(I+IOFF) = XNAMES(I)
        ENDDO
        NUM_TAGS(7) = NTAGS
        NUM_REPS(7) = 1
        IOFF = IOFF + NTAGS
C
        IOFF_FILTINFO = IOFF_GLBINFO + (NTAGS-1) + 1
        CALL EVT_FILTINFO_TAGS(NTAGS,XNAMES)
        GROUP_NAME(8) = 'TRIGFILT'
        DO I=1, NTAGS
          NTUP_NAMES(I+IOFF) = XNAMES(I)
        ENDDO
        NUM_TAGS(8) = NTAGS
        NUM_REPS(8) = 1
        IOFF = IOFF + NTAGS
C
        IOFF_Z_FIT = IOFF_FILTINFO + (NTAGS-1) + 1
        CALL EVT_Z_FIT_TAGS(NTAGS,XNAMES)
        GROUP_NAME(9) = 'Z_FIT'
        DO I=1, NTAGS
          NTUP_NAMES(I+IOFF) = XNAMES(I)
        ENDDO
        NUM_TAGS(9) = NTAGS
        NUM_REPS(9) = 1
        IOFF = IOFF + NTAGS
C
        IOFF_MUISA = IOFF_Z_FIT + (NTAGS-1) + 1
        CALL MUISA_TAGS(NTAGS,XNAMES,MAX)
        CALL TOP_DILEP_SETUP_TAGS(NTAGS,XNAMES,NTUP_NAMES(1+IOFF))
        LENC = TRULEN(NTUP_NAMES(1+IOFF))
        WRITE(CMAX,'(''[0,'',I3.3,'']'')') MAX
        NTUP_NAMES(1+IOFF) = NTUP_NAMES(1+IOFF)(1:LENC)
     &    //CMAX(1:TRULEN(CMAX))//':I'
        GROUP_NAME(10) = 'MUISA'
        NUM_TAGS(10) = NTAGS
        NUM_REPS(10) = MAX
        IOFF = IOFF + NTAGS
C
        IOFF_BBISA = IOFF_MUISA + (NTAGS-1)*MAX + 1
        CALL BBISA_TAGS(NTAGS,XNAMES,MAX)
        CALL TOP_DILEP_SETUP_TAGS(NTAGS,XNAMES,NTUP_NAMES(1+IOFF))
        LENC = TRULEN(NTUP_NAMES(1+IOFF))
        WRITE(CMAX,'(''[0,'',I3.3,'']'')') MAX
        NTUP_NAMES(1+IOFF) = NTUP_NAMES(1+IOFF)(1:LENC)
     &    //CMAX(1:TRULEN(CMAX))//':I'
        GROUP_NAME(11) = 'BBISA'
        NUM_TAGS(11) = NTAGS
        NUM_REPS(11) = MAX
        IOFF = IOFF + NTAGS
C
        IOFF_L2MUON = IOFF_BBISA + (NTAGS-1)*MAX + 1
        CALL L2MUON_TAGS(NTAGS,XNAMES,MAX)
        CALL TOP_DILEP_SETUP_TAGS(NTAGS,XNAMES,NTUP_NAMES(1+IOFF))
        LENC = TRULEN(NTUP_NAMES(1+IOFF))
        WRITE(CMAX,'(''[0,'',I3.3,'']'')') MAX
        NTUP_NAMES(1+IOFF) = NTUP_NAMES(1+IOFF)(1:LENC)
     &    //CMAX(1:TRULEN(CMAX))//':I'
        GROUP_NAME(12) = 'L2MUON'
        NUM_TAGS(12) = NTAGS
        NUM_REPS(12) = MAX
        IOFF = IOFF + NTAGS
C
        IOFF_L2JET = IOFF_L2MUON + (NTAGS-1)*MAX + 1
        CALL L2JET_TAGS(NTAGS,XNAMES,MAX)
        CALL TOP_DILEP_SETUP_TAGS(NTAGS,XNAMES,NTUP_NAMES(1+IOFF))
        LENC = TRULEN(NTUP_NAMES(1+IOFF))
        WRITE(CMAX,'(''[0,'',I3.3,'']'')') MAX
        NTUP_NAMES(1+IOFF) = NTUP_NAMES(1+IOFF)(1:LENC)
     &    //CMAX(1:TRULEN(CMAX))//':I'
        GROUP_NAME(13) = 'L2JET'
        NUM_TAGS(13) = NTAGS
        NUM_REPS(13) = MAX
        IOFF = IOFF + NTAGS
C
C
        NTVAR = IOFF
C
        CALL INRCP('top_dilep_rcp',IER)
        IF(IER.NE.0) THEN
          CALL ERRMSG('top_dilep','top_dilep','inrcp failed','w')
        ELSE
          CALL EZPICK('top_dilep_rcp')
          CALL EZERR(IER)
          CALL GTUNIT(777,UNIT,IER)
          IF(IER.EQ.0)CALL EZGET('ntuple_max_records', MAX_REC, IER)
          IF(IER.EQ.0)CALL EZGET('ntuple_record_size', LRECL, IER)
          IF(IER.EQ.0)CALL EZGET('ntuple_buffer_size', NWBUFF, IER)
          IF(IER.EQ.0)CALL EZGETS('TOP_DIRNAME',1,TOP_DIRNAME,L, IER)
          IF(IER.EQ.0)CALL EZGETS('NTFILE', 1, NTFILE, L, IER)
          IF(IER.EQ.0)CALL EZGETS('TITLE', 1, TITLE, L, IER)
          IF (IER.NE.0)CALL ERRMSG('error getting rcp parameters',
     &      'ee',' ','f')
          CALL EZRSET

          IQUEST(10) = MAX_REC
          CALL HROPEN(UNIT, TOP_DIRNAME, NTFILE, 'NQ', LRECL, IER)
          IF(IER.NE.0)CALL ERRMSG('TOP_DILEP', 'TOP_DILEP',
     &      'HROPEN failed for ntuple file', 'F')
          CALL HBSET('BSIZE', NWBUFF, IER)
          CALL HBNT(1,TITLE, ' ')
          IF(IQUEST(1).NE.0)
     &      CALL ERRMSG('TOP_DILEP','TOP_DILEP','HBNT failed', 'F')
C- Define columns
          K = 0
          NEXT_PTR = 0
          PTRFL = 0
          DO I = 1, NGROUPS
            CHBLOK = GROUP_NAME(I)
            CHFORM = ' '
            DO J = K + 1, K + NUM_TAGS(I)
              LENC = TRULEN(NTUP_NAMES(J))
              LEN_CHFORM = TRULEN(CHFORM)
              IF (LENC+LEN_CHFORM+3.GT.256) THEN
                CALL HBNAME(1, CHBLOK, INTUP_VAR(PTRFL+1), CHFORM)
                CHFORM = ' '
                PTRFL  = NEXT_PTR
              ENDIF
              IF(CHFORM.NE.' ')CHFORM = CHFORM(1:TRULEN(CHFORM))//','
              CHFORM = CHFORM(1:TRULEN(CHFORM))//
     &                 NTUP_NAMES(J)(1:TRULEN(NTUP_NAMES(J)))
              IF(INDEX(NTUP_NAMES(J),':').EQ.0)
     &            CHFORM = CHFORM(1:TRULEN(CHFORM))//':R'
              IF (J.EQ.K+1) THEN
                NEXT_PTR = NEXT_PTR + 1
              ELSE
                NEXT_PTR = NEXT_PTR + NUM_REPS(I)
              ENDIF
            ENDDO
            IF(TRULEN(CHFORM).EQ.LEN(CHFORM))
     &        CALL ERRMSG('TOP_DILEP', 'TOP_DILEP',
     &        'Column too big', 'F')
            CALL HBNAME(1, CHBLOK, INTUP_VAR(PTRFL+1), CHFORM)
            CHFORM = ' '
            PTRFL  = NEXT_PTR
            K = K + NUM_TAGS(I)
          ENDDO
        ENDIF
      ENDIF
C
C ****  Compute all relevant qualitities for this event
C
      NUM_INPUT = NUM_INPUT + 1
      TOP_DILEP = TOP_DILEP_ANALYSIS()
      IF (.NOT.TOP_DILEP) THEN
        GOTO 999
      ENDIF
C
C ****  FILL NTUPLE
C
      CALL VZERO(NTUP_VAR,5000)
C
C ****  RUN time information
C
      CALL EVT_GENINFO(NVAR,XDATA)
      IOFF = IOFF_GENINFO
      CALL UCOPY(XDATA,NTUP_VAR(1+IOFF),NVAR)
C
C ****  Missing Et information
C
      CALL MET_INFO(NVAR,XDATA)
      IOFF = IOFF_MET
      CALL UCOPY(XDATA,NTUP_VAR(1+IOFF),NVAR)
C
C ****  Electron information
C
      CALL ELEC_INFO(NVAR,XDATA)
      IOFF = IOFF_ELEC
      CALL UCOPY(XDATA,NTUP_VAR(1+IOFF),NVAR)
C
C ****  Photon information
C
      CALL PHOT_INFO(NVAR,XDATA)
      IOFF = IOFF_PHOT
      CALL UCOPY(XDATA,NTUP_VAR(1+IOFF),NVAR)
C
C ****  Jets info
C
      CALL JETS_INFO(NVAR,XDATA)
      IOFF = IOFF_JETS
      CALL UCOPY(XDATA,NTUP_VAR(1+IOFF),NVAR)
C
C ****  Muon info
C
      CALL MUON_INFO(NVAR,XDATA)
      IOFF = IOFF_MUON
      CALL UCOPY(XDATA,NTUP_VAR(1+IOFF),NVAR)
C
C ****  Global quantities in event info
C
      CALL EVT_GLBINFO(NVAR,XDATA)
      IOFF = IOFF_GLBINFO
      CALL UCOPY(XDATA,NTUP_VAR(1+IOFF),NVAR)
C
C ****  TRIGGER/FILTER quantities in info
C
      CALL EVT_FILTINFO(NVAR,XDATA)
      IOFF = IOFF_FILTINFO
      CALL UCOPY(XDATA,NTUP_VAR(1+IOFF),NVAR)
C
C ****  Z_FIT quantities in info
C
      CALL Z_FIT_INFO(NVAR,XDATA)
      IOFF = IOFF_Z_FIT
      CALL UCOPY(XDATA,NTUP_VAR(1+IOFF),NVAR)
C
C ****  Muon Isajet info
C
      CALL MUISA_INFO(NVAR,XDATA)
      IOFF = IOFF_MUISA
      CALL UCOPY(XDATA,NTUP_VAR(1+IOFF),NVAR)
C
C ****  BBar Isajet info
C
      CALL BBISA_INFO(NVAR,XDATA)
      IOFF = IOFF_BBISA
      CALL UCOPY(XDATA,NTUP_VAR(1+IOFF),NVAR)
C
C ****  Level 2 Muon info
C
      CALL L2MUON_INFO(NVAR,XDATA)
      IOFF = IOFF_L2MUON
      CALL UCOPY(XDATA,NTUP_VAR(1+IOFF),NVAR)
C
C ****  Level 2 Jet info
C
      CALL L2JET_INFO(NVAR,XDATA)
      IOFF = IOFF_L2JET
      CALL UCOPY(XDATA,NTUP_VAR(1+IOFF),NVAR)
C
C ****  Fill ntuple
C
      NUM_NTUPLE = NUM_NTUPLE+ 1
      CALL HFNT(1)
  999 RETURN
C======================================================================
      ENTRY TOP_DILEP_FIN
C-
C close ntuple
      IF(IQUEST(1).EQ.0)CALL HROUT(1, ICYCLE, ' ')
      IF(IQUEST(1).EQ.0)CALL HREND(TOP_DIRNAME)
      IF(IQUEST(1).NE.0)
     &  CALL ERRMSG('TOP_DILEP', 'TOP_DILEP',
     &  'Error closing ntuple', 'F')
C-
C- Print statistics
C-
      LUN = SSUNIT()
      PRINT 500, NUM_INPUT, NUM_NTUPLE
      WRITE(LUN,500)NUM_INPUT, NUM_NTUPLE
  500 FORMAT(/' TOP_DILEP package statistics:'/
     &  /1X,I8,' Events processed'
     &  /1X,I8,' Events stored in Ntuple')
      TOP_DILEP_FIN = .TRUE.
      END
