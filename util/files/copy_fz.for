      PROGRAM COPY_FZ
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Copy zebra files via FZIN/FZOUT. Can be use to split
C-                         one file into many, concatenate many into one, add
C-                         Begin Run records, change (correct) run numbers and
C-                         renumber event numbers.
C-
C-   Inputs  : Zebra FZ event files, one or more
C-   Outputs : Zebra FZ event files, one or more
C-   Controls: COPY_FZ_RCP  Switches
C-
C-   Created  25-AUG-1989   Alan M. Jonckheere
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER INLUN,OUTLUN,TXTLUN,TMPLUN,IEVENT
C
C--     ZEBRA VARIABLES
C------------------------------------------------------------------------
C  ZEBCOM is the main zebra common block for event data storage
C
      INTEGER NNQ,NREF
      PARAMETER (NNQ=1000000)
      PARAMETER (NREF=9)
      COMMON/ZEBCOM/IXCOM,IXMAIN,IXDVR,FENCE,LHEAD,LHEADR,LREF,
     &  ZSTOR,ENDZS
      INTEGER IXCOM    ! store number
     &       ,IXMAIN   ! event division number
     &       ,IXDVR    ! run division number
      INTEGER FENCE(8),LREF(NREF),ZSTOR(NNQ),ENDZS
      INTEGER LHEAD     ! pointer to event HEAD bank
      INTEGER LHEADR    ! pointer to begin run HEAD bank
      REAL Q(NNQ)
      INTEGER IQ(NNQ),LQ(NNQ)
      EQUIVALENCE (LHEAD,LQ(1)),(LQ(9),IQ(1),Q(1))
C------------------------------------------------------------------------
C      INCLUDE 'D0$INC:ZEBSTP.INC'
C------------------------------------------------------------------------
      INCLUDE 'D0$INC:QUEST.INC'
C------------------------------------------------------------------------
      INTEGER ixwipe
C------------------------------------------------------------------------
C
      CHARACTER*132 INFILE,OUTFILE
      CHARACTER*132 BRR_FILE,INF,outf
      CHARACTER*40 skp_copy
      INTEGER nskip,nevent
      INTEGER NUH,IUHEAD(20)
      INTEGER status,len
      LOGICAL FIX_BRR,MAKE_BRR,ADD_BRR
      LOGICAL KEEP_BRR
      INTEGER renum_run,renum_evt
      CHARACTER*132 copy_fz_log
      LOGICAL inp_tape
      CHARACTER*16 outp_form,inp_form,outpf,inpf
      CHARACTER*2 inform,outform
      INTEGER print_do,print_skip,nlinks,drop_links(30)
C
      INTEGER i
      INTEGER GZISAE,LISAE
      CHARACTER*4 doing
      LOGICAL firstout
      INTEGER trulen
c
      INTEGER levnt
      INTEGER nfile,idxfile
      INTEGER lastrun
      LOGICAL last_file
C
      CHARACTER*132 MSG_OUT
      INTEGER ptr,val,type
      CHARACTER*1 cval
      LOGICAL OK
C
      CHARACTER*4 CHOPT
      INTEGER ILEN
C
      DATA INFILE/' '/,OUTFILE/' '/
      DATA BRR_FILE/' '/,skp_copy/' '/,INF/' '/,outf/' '/
      DATA nskip/0/,nevent/99999/
      DATA NUH/0/
      DATA FIX_BRR/.FALSE./,MAKE_BRR/.FALSE./,ADD_BRR/.FALSE./
      DATA KEEP_BRR/.TRUE./
      DATA renum_run/-1/,renum_evt/-1/
      DATA copy_fz_log/'NL:'/
      DATA inp_tape/.FALSE./
      DATA outp_form/'EXCHANGE'/,inp_form/'EXCHANGE'/
      DATA inform/'X'/,outform/'X'/
      DATA print_do/5/,print_skip/100/,nlinks/0/,drop_links/30*0/
      DATA firstout/.true./
      DATA levnt/0/
      DATA nfile/0/,idxfile/0/
      DATA lastrun/-1/
      DATA last_file/.FALSE./
      DATA val/-1/
      DATA cval/' '/
C----------------------------------------------------------------------
C
C ****  Init zebra stores
      CALL MZEBRA(0)
      CALL INZCOM(0)
      CALL inzstp
C
C ****  Get I/O unit numbers
      CALL gtunit(100,TXTLUN,status)
      CALL gtunit(100,INLUN,status)
      CALL gtunit(100,OUTLUN,status)
C
C ****  Get RCP switches
      CALL inrcp('copy_fz_rcp',status)
      IF ( status.NE.0 ) THEN
        WRITE(msg_out,'(a)')
     &    ' Could not open RCP file COPY_FZ_RCP: try COPY_FZ.RCP'
        WRITE(6,*) msg_out
        WRITE(TXTLUN,*) msg_out
C
C ****  Try COPY_FZ.RCP (if user has one but didn't define logical
        CALL inrcp('copy_fz.rcp',status)
        IF ( status.NE.0 ) THEN
          WRITE(msg_out,'(a)')
     &      ' Could not open RCP file COPY_FZ.RCP: using all defaults'
          WRITE(6,*) msg_out
          WRITE(TXTLUN,*) msg_out
          WRITE(msg_out,'(a)')
     &      ' Input format will be determined automatically'
          WRITE(6,*) msg_out
          WRITE(TXTLUN,*) msg_out
          WRITE(msg_out,'(a)')
     &      ' Output format will be EXCHANGE'
          WRITE(6,*) msg_out
          WRITE(TXTLUN,*) msg_out
          GOTO 10
        ELSE
          WRITE(msg_out,'(a)')
     &      ' Found RCP file COPY_FZ.RCP'
          WRITE(6,*) msg_out
          WRITE(TXTLUN,*) msg_out
        ENDIF
      ELSE
        WRITE(msg_out,'(a)')
     &      ' Found RCP file COPY_FZ_RCP'
        WRITE(6,*) msg_out
        WRITE(TXTLUN,*) msg_out
      ENDIF
      CALL ezpick('copy_fz_rcp')    ! select bank
C
C ****  Text output file
      CALL ezgets('COPY_FZ_LOG',1,copy_fz_log,len,status)
      IF ( status.NE.0 ) THEN
        WRITE(msg_out,'('' COPY_FZ_LOG not found: '',a,'' assumed'')')
     &      copy_fz_log(1:trulen(copy_fz_log))
        WRITE(6,*) msg_out
        WRITE(TXTLUN,*) msg_out
      ENDIF
      CALL d0open(txtlun,copy_fz_log,'FOL',ok)
C
C ****  Number of events to skip between printing an event
      CALL ezget_i('PRINT_DO',print_do,status)
      IF ( status.NE.0 ) THEN
        WRITE(msg_out,'('' PRINT_DO not found:'',i4,'' assumed'')')
     &      print_do
        WRITE(6,*) msg_out
        WRITE(TXTLUN,*) msg_out
      ENDIF
      CALL ezget_i('PRINT_SKIP',print_skip,status)
      IF ( status.NE.0 ) THEN
        WRITE(msg_out,'('' PRINT_SKIP not found:'',i4,'' assumed'')')
     &      print_skip
        WRITE(6,*) msg_out
        WRITE(TXTLUN,*) msg_out
      ENDIF
C
C ****  Banks to drop (by LHEAD link numbers)
      CALL ezgeta('DROP_LINKS',0,0,0,nlinks,status)
      IF ( status.NE.0 ) THEN
        WRITE(msg_out,'('' DROP_LINKS not found:'',i4,'' assumed'')')
     &      nlinks
        WRITE(6,*) msg_out
        WRITE(TXTLUN,*) msg_out
      ELSE
        CALL ezget_iarr('DROP_LINKS',drop_links,status)
        WRITE(msg_out,'('' Dropping'',i3,'' Links:'')') nlinks
        WRITE(6,*) msg_out
        WRITE(txtlun,*) msg_out
        WRITE(msg_out,'(15i3)') (drop_links(i),i=1,min(15,nlinks))
        WRITE(6,*) msg_out
        WRITE(txtlun,*) msg_out
        IF ( nlinks.GT.15 ) THEN
          WRITE(msg_out,'(15i3)') (drop_links(i),i=16,nlinks)
          WRITE(6,*) msg_out
          WRITE(txtlun,*) msg_out
        ENDIF
      ENDIF
C
C ****  Keep all Begin Run Records?
      CALL ezget_l('KEEP_BRR',keep_brr,status)
      IF ( status.NE.0 ) THEN
        WRITE(msg_out,'('' KEEP_BRR not found:'',L1,'' assumed'')')
     &      keep_brr
        WRITE(6,*) msg_out
        WRITE(TXTLUN,*) msg_out
      ENDIF
C
C ****  Fix Begin Run Records?
      CALL ezget_l('FIX_BRR',fix_brr,status)
      IF ( status.NE.0 ) THEN
        WRITE(msg_out,'('' FIX_BRR not found:'',L1,'' assumed'')')
     &      fix_brr
        WRITE(6,*) msg_out
        WRITE(TXTLUN,*) msg_out
      ENDIF
      IF ( fix_brr ) THEN
        CALL ezget_l('MAKE_BRR',make_brr,status)
        IF ( status.NE.0 ) THEN
          WRITE(msg_out,'('' MAKE_BRR not found:'',L1,'' assumed'')')
     &        make_brr
          WRITE(6,*) msg_out
          WRITE(TXTLUN,*) msg_out
        ENDIF
        IF ( .NOT.make_brr ) THEN
          CALL ezget_l('ADD_BRR',add_brr,status)
          IF ( status.NE.0 ) THEN
            WRITE(msg_out,'('' ADD_BRR not found:'',L1,'' assumed'')')
     &          add_brr
            WRITE(6,*) msg_out
            WRITE(TXTLUN,*) msg_out
          ENDIF
          IF ( add_brr ) THEN
            CALL ezgets('BRR_FILE',1,brr_file,len,status)
            IF ( status.NE.0 ) THEN
              WRITE(msg_out,
     &            '('' BRR_File not found: Setting ADD_BRR to .F.'')')
              WRITE(6,*) msg_out
              WRITE(TXTLUN,*) msg_out
              add_brr = .FALSE.
            ENDIF
          ENDIF
        ENDIF
      ENDIF
C
C ****  Renumber Run numbers?
      CALL ezget_i('RENUM_RUN',renum_run,status)
      IF ( status.NE.0 ) THEN
        WRITE(msg_out,'('' RENUM_RUN not found:'',i4,'' assumed'')')
     &      renum_run
        WRITE(6,*) msg_out
        WRITE(TXTLUN,*) msg_out
      ENDIF
C
C ****  Renumber Event numbers?
      CALL ezget_i('RENUM_EVT',renum_evt,status)
      IF ( status.NE.0 ) THEN
        WRITE(msg_out,'('' RENUM_EVT not found:'',i4,'' assumed'')')
     &      renum_evt
        WRITE(6,*) msg_out
        WRITE(TXTLUN,*) msg_out
      ENDIF
C
C ****  Input tape flag?
      CALL ezget('INP_TAPE',inp_tape,status)
      IF(status.NE.0 ) THEN
        WRITE(msg_out,'('' INP_TAPE not found:'',L1,'' assumed'')')
     &      inp_tape
        WRITE(6,*) msg_out
        WRITE(TXTLUN,*) msg_out
      ENDIF
C
C ****  Input and output file formats
      CALL ezgets('INP_FORM',1,inpf,len,status)
      IF ( status.NE.0 ) THEN
        WRITE(msg_out,'('' INP_FORM not found: '',a,'' assumed'')')
     &      inp_form(1:trulen(inp_form))
        WRITE(6,*) msg_out
        WRITE(TXTLUN,*) msg_out
      ELSE
        inp_form = inpf
      ENDIF
      IF ( INP_FORM.EQ.'EXCHANGE' ) THEN
        inform = 'X'
      ELSEIF (INP_FORM.EQ.'GEANT') THEN
        inform = 'G'
      ELSE
        inform = ' '
      ENDIF
      IF( INP_TAPE ) inform = 'T'//inform
c
      CALL ezgets('OUTP_FORM',1,outpf,len,status)
      IF ( status.NE.0 ) THEN
        WRITE(msg_out,'('' OUTP_FORM not found: '',a,'' assumed'')')
     &      outp_form(1:trulen(outp_form))
        WRITE(6,*) msg_out
        WRITE(TXTLUN,*) msg_out
      ELSE
        outp_form = outpf
      ENDIF
      IF ( OUTP_FORM.EQ.'EXCHANGE' ) THEN
        outform = 'X'
        WRITE(msg_out,'(a)')
     &      ' Output format will be EXCHANGE'
        WRITE(6,*) msg_out
        WRITE(TXTLUN,*) msg_out
      ELSEIF ( OUTP_FORM.EQ.'GEANT' ) THEN
        outform = 'G'
        WRITE(msg_out,'(a)')
     &      ' Output format will be GEANT'
        WRITE(6,*) msg_out
        WRITE(TXTLUN,*) msg_out
      ELSE
        outform = ' '
        WRITE(msg_out,'(a)')
     &      ' Output format will be NATIVE'
        WRITE(6,*) msg_out
        WRITE(TXTLUN,*) msg_out
      ENDIF
C
C ****  File names from RCP?
      CALL ezgeta('FILE_LIST',0,0,0,nfile,status)
      IF ( mod(nfile,4).ne.0 ) THEN
        nfile = nfile/4 + 1
      ELSE
        nfile = nfile/4
      ENDIF
      IF ( status.NE.0 .OR. nfile.LE.0 ) THEN
        WRITE(msg_out,'('' No files given: input interactively'')')
        WRITE(6,*) msg_out
        WRITE(txtlun,*) msg_out
      ENDIF
      idxfile = 0
C
C ****  Main processing loop
C
      last_file = .FALSE.
      ptr = 1
   10 levnt = 0
      IF ( (nfile.GT.0 .AND. idxfile+1.gt.nfile) .OR. LAST_FILE )
     &    GOTO 998
      IF ( nfile.GT.0 ) THEN
        inf = ' '
        CALL ezget_next_value_type('FILE_LIST',val,inf,type,len,
     &      status,ptr)
        IF ( TYPE.LE.10 .OR. status.NE.0 ) THEN
          IF ( status.EQ.1 .AND. .NOT.last_file ) THEN
            last_file = .TRUE.
          ELSE
            WRITE(msg_out,
     &          '('' Illegal or missing file name: ABORTING'')')
            WRITE(6,*) msg_out
            WRITE(TXTLUN,*) msg_out
            GOTO 998
          ENDIF
        ENDIF
C&IF VAXVMS
        CALL str$upcase(inf,inf)                ! UPCASE the string
C&ENDIF
      ELSE
        CALL get_str('Input Data File',inf)
      ENDIF
      WRITE(msg_out,'('' Input: '',a)')
     &      INF(1:max(1,trulen(inf)))
      WRITE(6,*) msg_out
      WRITE(TXTLUN,*) msg_out
      IF ( INF.EQ.'END' .OR. INF.EQ.'QUIT' .OR. INF.EQ.' ' ) GOTO 998
      IF ( inf.NE.infile ) THEN
        IF ( infile.NE.' ' ) THEN
          CALL FZENDI(INLUN,'TU')
          CLOSE (INLUN)
        ENDIF
        infile = inf
C--------------------------------------------------------------------
C   Replace file open code with new scheme for exchange mode
C
        CALL EVOPIN(INFILE,INFORM,INLUN,OK)
        IF (.NOT. OK) GO TO 990
C--------------------------------------------------------------------
C
C ****  BRR needs fixing?
        IF ( FIX_BRR ) THEN
C
C ****  Modify 1st event record into a BRR?
          IF ( make_brr ) THEN
            CALL evtin(inlun,status)
            IF ( status.EQ.0 ) THEN
              WRITE(6,*) ' Make 1st Event record into a BRR record'
              WRITE(txtlun,*)
     &            ' Make 1st Event record into a BRR record'
              IQ(LHEAD+1) = 1001            ! Make it a BRR
              CALL uctoh('ISAJ',IQ(LHEAD+2),4,4)
              CALL uctoh('BEG ',IQ(LHEAD+3),4,4)
C
C ****  Drop ISAE
              LISAE = GZISAE()
              IF ( LISAE.GT.0 ) THEN
                CALL MZDROP(IXCOM,LISAE,'L')
              ENDIF
              IXWIPE = IXCOM+IXDVR
              CALL MZWIPE(IXWIPE)
              CALL MZCOPY(IXMAIN,LHEAD,IXDVR,LHEADR,1,' ')
              IF ( renum_run.GT.0 ) THEN
                iq(lheadr+6) = renum_run
              ENDIF
              IF ( renum_evt.GT.0 ) THEN
                iq(lheadr+9) = 0
              ENDIF
            ELSEIF ( status.EQ.1 ) THEN
              WRITE(txtlun,*) ' Already BRR'
              WRITE(6,*) ' Already BRR'
              IF ( renum_run.GT.0 ) THEN
                iq(lheadr+6) = renum_run
              ENDIF
              IF ( renum_evt.GT.0 ) THEN
                iq(lheadr+9) = 0
              ENDIF
            ELSE
              WRITE(txtlun,*) ' Problem: ',status
              WRITE(6,*) ' Problem: ',status
              GOTO 998
            ENDIF
          ELSE
C
C ****  add BRR from another file?
            IF ( ADD_BRR ) THEN
              WRITE(msg_out,'('' BRR input: '',a)')
     &            BRR_FILE(1:max(1,trulen(BRR_FILE)))
              WRITE(6,*) msg_out
              WRITE(TXTLUN,*) msg_out
              CALL gtunit(100,TMPLUN,status)
C--------------------------------------------------------------------
C   Replace file open code with new scheme for exchange mode
C
              CALL EVOPIN(BRR_FILE,INFORM,TMPLUN,OK)
              IF (.NOT. OK) GO TO 992
C--------------------------------------------------------------------
              CALL evtin(tmplun,status)
              IF ( status.NE.1 ) THEN
                WRITE(txtlun,*) ' -E- NO BRR on BRR file'
                WRITE(6,*) ' -E- NO BRR on BRR file'
                CALL fzendi(TMPLUN,'TU')
                CLOSE (TMPLUN)
                CALL rlunit(100,TMPLUN,status)
                GOTO 998
              ENDIF
              CALL fzendi(TMPLUN,'TU')
              CLOSE (TMPLUN)
              CALL rlunit(100,TMPLUN,status)
c
              IF ( renum_run.GT.0 ) THEN
                iq(lheadr+6) = renum_run
              ENDIF
              IF ( renum_evt.GT.0 ) THEN
                iq(lheadr+9) = 0
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDIF
c
      IF ( nfile.GT.0 ) THEN
        outf = ' '
        CALL ezget_next_value_type('FILE_LIST',val,outf,type,len,
     &      status,ptr)
        IF ( TYPE.LE.10 .OR. status.NE.0 ) THEN
          IF ( status.EQ.1 .AND. .NOT.last_file ) THEN
            last_file = .TRUE.
          ELSE
            WRITE(msg_out,
     &          '('' Illegal or missing file name: ABORTING'')')
            WRITE(6,*) msg_out
            WRITE(TXTLUN,*) msg_out
            GOTO 998
          ENDIF
        ENDIF
C&IF VAXVMS
        CALL str$upcase(outf,outf)                ! UPCASE the string
C&ENDIF
      ELSE
        CALL get_str('Output Data File',outf)
      ENDIF
      WRITE(msg_out,'('' Output: '',a)')
     &      OUTF(1:max(1,trulen(outf)))
      WRITE(6,*) msg_out
      WRITE(TXTLUN,*) msg_out
      IF ( outf.NE.outfile ) THEN
        IF ( outfile.NE.' ' ) THEN
          CALL FZENDO(OUTLUN,'TU')
          CLOSE (outlun)
        ENDIF
        IF ( outf.EQ.' ' ) GOTO 998
        outfile = outf
C--------------------------------------------------------------------
C   Replace file open code with new scheme for exchange mode
C
        CHOPT = 'OU'
        IF(OUTFORM.EQ.'X') CHOPT='XO'
        IF(OUTFORM.EQ.'G') CHOPT='GO'
        CALL D0OPEN(OUTLUN,OUTFILE,CHOPT,OK)
        CALL XZRECL(ILEN,CHOPT)
        CALL FZFILE(OUTLUN,ILEN,CHOPT)

C--------------------------------------------------------------------
        firstout = .true.
      ENDIF
      IF ( keep_brr ) firstout = .true.
C
C ****  Number of events to skip/copy
C
      IF ( nfile.GT.0 ) THEN
C ****  Number of events to skip
        CALL ezget_next_value_type('FILE_LIST',nskip,cval,type,len,
     &      status,ptr)
        IF ( TYPE.NE.1 .OR. status.NE.0 ) THEN
          IF ( status.EQ.1 .AND. .NOT.last_file ) THEN
            last_file = .TRUE.
          ELSE
            WRITE(msg_out,
     &          '('' Illegal or missing NSKIP: ABORTING'')')
            WRITE(6,*) msg_out
            WRITE(TXTLUN,*) msg_out
            GOTO 998
          ENDIF
        ENDIF
C ****  Number of events to copy
        CALL ezget_next_value_type('FILE_LIST',nevent,cval,type,len,
     &      status,ptr)
        IF ( TYPE.NE.1 .OR. status.NE.0 ) THEN
          IF ( status.EQ.1 .AND. .NOT.last_file ) THEN
            last_file = .TRUE.
          ELSE
            WRITE(msg_out,
     &          '('' Illegal or missing NEVENT: ABORTING'')')
            WRITE(6,*) msg_out
            WRITE(TXTLUN,*) msg_out
            GOTO 998
          ENDIF
        ENDIF
      ELSE
        CALL get_str('Events to skip/copy',skp_copy)
        READ(skp_copy,*) nskip,nevent
      ENDIF
      WRITE(TXTLUN,*) ' Events: skip/copy ',nskip,nevent
      WRITE(6,*) ' Events: skip/copy ',nskip,nevent
C
C ****  Bump index for next file
      idxfile = idxfile + 1
C
C--     LOOP OVER EVENTS
      IEVENT = 0
  100 CALL evtin(inlun,status)
      IF ( STATUS.GT.3 ) THEN           ! 4 = System End-of-file
C                                       ! 5 = System End-of-data
C                                       ! 6 = attempt to read past end-of-data
        levnt = 0
        WRITE(TXTLUN,*) ' System End of file',IEVENT,status
        WRITE(6,*) ' System End of file',IEVENT,status
        GOTO 10
      ELSEIF ( STATUS.EQ.3 ) THEN       ! 3 = Zebra End-of-File
        levnt = 0
        WRITE(TXTLUN,*) ' ZEBRA  END of FILE',IEVENT
        WRITE(6,*) ' ZEBRA  END of FILE',IEVENT
        GOTO 100
      ELSEIF ( status.EQ.2 ) THEN       ! 2 = Zebra end-run-record
        IF ( lhead.EQ.0 ) THEN
          levnt = 0
          WRITE(TXTLUN,*) ' ZEBRA  END of RUN ',IEVENT
          WRITE(6,*) ' ZEBRA  END of RUN ',IEVENT
          GOTO 100
        ELSE
          levnt = 0
          WRITE(TXTLUN,*) ' D0     END of RUN ',IEVENT
          WRITE(6,*) ' D0     END of RUN ',IEVENT
          IF ( .NOT.keep_brr ) GOTO 100
        ENDIF
      ELSEIF ( STATUS.EQ.1 ) THEN
        IF ( LHEAD.EQ.0 ) THEN
          WRITE(TXTLUN,*) ' ZEBRA  BEG of RUN ',IEVENT
          WRITE(6,*) ' ZEBRA  BEG of RUN ',IEVENT
          GOTO 100
        ELSE
          WRITE(TXTLUN,*) ' D0     BEG of RUN ',IEVENT
          WRITE(6,*) ' D0     BEG of RUN ',IEVENT
          IF ( .NOT.keep_brr ) GOTO 100
        ENDIF
      ELSEIF ( status.LT.0 ) THEN
        levnt = 0
        WRITE(txtlun,*) ' READ ERROR ',iquest(1),ievent
        WRITE(6,*) ' READ ERROR ',iquest(1),ievent
        IF ( iquest(1).GT.-10 ) goto 100   ! Retry
        GOTO 10
      ENDIF
      IF ( (FIRSTOUT.OR.(STATUS.EQ.1.AND.keep_brr)) .AND.
     &            LHEADR.GT.0 ) THEN
        levnt = 0
        IF ( renum_run.GT.0 ) THEN
          iq(lheadr+6) = renum_run
        ENDIF
        IF ( renum_evt.GT.0 ) THEN
          iq(lheadr+9) = 0
        ENDIF
        IF ( LHEAD.NE.0 .AND. nlinks.GT.0 ) THEN
          DO I =  1, nlinks
            IF ( i.LE.iq(lhead-2) .and. LQ(LHEAD-drop_links(I)).NE.0 )
     &          CALL MZDROP(IXCOM,LQ(LHEAD-drop_links(I)),'L')
          ENDDO
        ENDIF
        WRITE(6,*) ' Write Begin Run Record'
        WRITE(TXTLUN,*) ' Write Begin Run Record'
        CALL FZOUT(OUTLUN,IXDVR,LHEADR,1,' ',2,0,IUHEAD)
        firstout = .false.
      ENDIF
C
C ****  write data record
      IF ( status.EQ.1 ) GOTO 100
c
      IEVENT = IEVENT + 1
      IF ( ievent.LE.nskip ) THEN
        doing = 'skip'
      ELSE
        IF ( doing.EQ.'skip' ) levnt = 0
        doing = 'copy'
      ENDIF
      IF ( renum_run.GT.0 ) THEN
        iq(lhead+6) = renum_run
        iq(lhead+12) = renum_run
      ENDIF
      IF ( renum_evt.GT.0 ) THEN
        iq(lhead+9) = renum_evt
        renum_evt = renum_evt + 1
      ENDIF
      IF ( iq(lhead+6).ne.lastrun ) THEN
        lastrun = iq(lhead+6)
        levnt = 0
      ENDIF
C
      levnt = levnt + 1
      IF ( mod(levnt-1,print_skip).lt.print_do ) THEN
        WRITE(6,12) IEVENT,IQ(LHEAD+9),IQ(LHEAD+6),
     &      IQ(LHEAD+7),IQ(LHEAD+8),doing
C
        WRITE(TXTLUN,12) IEVENT,IQ(LHEAD+9),IQ(LHEAD+6),
     &      IQ(LHEAD+7),IQ(LHEAD+8),doing
C
   12   FORMAT(' EVENT: ',I10,' OUTPUT EVENT/RUN:',2I10,
     &      ' INPUT EVENT/RUN:',2I10,3X,A)
C
      ENDIF
C
      IF ( ievent.GT.nskip .AND. ievent.LE.(nevent+nskip) ) THEN
        IF ( LHEAD.NE.0 .AND. nlinks.GT.0 ) THEN
          DO I =  1, nlinks
            IF ( i.LE.iq(lhead-2) .and. LQ(LHEAD-drop_links(I)).NE.0 )
     &          CALL MZDROP(IXCOM,LQ(LHEAD-drop_links(I)),'L')
          ENDDO
        ENDIF
        CALL FZOUT(OUTLUN,IXMAIN,LHEAD,1,' ',2,NUH,IUHEAD)
      ENDIF
C
C--     GO TO BEGINNING OF LOOP
      IF ( IEVENT.LT.(NEVENT+nskip) ) GO TO 100
      GOTO 10
C
C ****  Error opening file
  990 WRITE(msg_out,'('' Could not open INFILE '',a)')
     &    infile(1:max(1,trulen(infile)))
      WRITE (6,*) msg_out
      GOTO 10
  992 WRITE(msg_out,'('' Could not open BRR_FILE '',a)')
     &    brr_file(1:max(1,trulen(infile)))
      WRITE (6,*) msg_out
      GOTO 10
C
C--     FINISHED
  998 CONTINUE
      CALL FZENDI(INLUN,'TU')
      CLOSE (INLUN)
      CALL rlunit(100,INLUN,status)
      CALL FZENDO(OUTLUN,'TU')
      CLOSE (outlun)
      CALL rlunit(100,OUTLUN,status)
      CLOSE (txtlun)
      CALL rlunit(100,txtlun,status)
  999 CONTINUE
      END
