      PROGRAM MAKE_NT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : convert micro DST to ntuple
C-
C-   Inputs  : uDST file
C-   Outputs : NT4 file
C-
C-   Created  16-OCT-1992   Ulrich Heintz
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      INTEGER JER,IER,MAXDIM,NENTRIES,NDIM,I,J,K,IENTFILE,ICYCLE
      INTEGER GET_INDEX,NTID,NCUT,NMAXCUT,NACC,IENT,IMOD,NACCMAX,NFILE
      INTEGER IFILE,ILEN,LREC,LIN,LOUT,LENGTH,IXWIPE,LUDST,GZUDST
      INTEGER PRUN,PFILT,FILTERWORDS(4),NFILTER,NFILTMAX,IRUN
      PARAMETER ( NMAXCUT = 10, MAXDIM = 512, NFILE = 100, NFILTMAX=32 )
      INTEGER  INDX(NMAXCUT)
      REAL    CUTVALUE(NMAXCUT),XDATA(MAXDIM)
      CHARACTER*1   RELATION(NMAXCUT),CHOPT*4
      CHARACTER*8   NAMES(MAXDIM), CUTVAR(NMAXCUT)
      CHARACTER*15  MSG
      CHARACTER*80  FILTER(NFILTMAX)
      CHARACTER*80  TITLE,STRING,INFILE(NFILE),OUTFILE,CUTS(NMAXCUT)
      LOGICAL       PASS,OK,XMODE,FILTSET
      INTEGER*4 HANDLER
      EXTERNAL  GET_INDEX,FILTSET
C&IF VAXVMS
      EXTERNAL  HANDLER
C&ENDIF
C----------------------------------------------------------------------
C&IF VAXVMS
      CALL LIB$ESTABLISH(HANDLER)
C&ENDIF
C
C ****  Initialize HBOOK and ZEBRA
C
      CALL INZBRA
      CALL INZCOM(2)
      CALL INPAWC
C
C **** get RCP parameters
C
      CALL ERRMAX(' ',-1,0)
      CALL INRCP('MAKE_NT_RCP',IER)
      IF(IER.NE.0)CALL ERRMSG('MAKE_NT_RCP','MAKE_NT','bank not found',
     &  'F')
      CALL EZPICK('MAKE_NT_RCP')
      IF(IER.EQ.0)CALL EZ_GET_CHARS('INPUT_FILES',IFILE,INFILE,IER)
      IF(IFILE.GT.NFILE)THEN
        CALL ERRMSG('MAKE_NT_RCP','MAKE_NT','too many input files','F')
      ENDIF
      IF(IER.EQ.0)CALL EZGETS('OUTPUT_FILE',1,OUTFILE,LENGTH,IER)
      IF(IER.EQ.0)CALL EZGET('NTUPLE_ID',NTID,IER)
      IF(IER.EQ.0)CALL EZGET('N_EVENTS',NACCMAX,IER)
      IF(IER.EQ.0)CALL EZ_GET_CHARS('CUTS',NCUT,CUTS,IER)
      IF(IER.EQ.0)THEN
        IF (NCUT .GT. NMAXCUT) THEN
          CALL ERRMSG('MAKE_NT_RCP','MAKE_NT','too many cuts','F')
        ENDIF
        DO I=1,NCUT
          K=MAX(INDEX(CUTS(I),'>'),INDEX(CUTS(I),'='),INDEX(CUTS(I),
     &      '<'))
          IF(K.GT.9)THEN
            CALL ERRMSG('MAKE_NT_RCP','MAKE_NT',
     &        'cut variable has more than 8 characters: '//CUTS(I),'W')
          ENDIF
          CALL UPCASE (CUTS(I)(:K-1), CUTVAR(I))
          RELATION(I)=CUTS(I)(K:K)
          IF ((RELATION(I) .NE. '<') .AND. (RELATION(I) .NE. '=') .AND.
     &        (RELATION(I) .NE. '>' )) THEN
            CALL ERRMSG('MAKE_NT_RCP','MAKE_NT',
     &        'invalid cut operator: '//CUTS(I),'F')
          ENDIF
          READ(CUTS(I)(K+1:),13)CUTVALUE(I)
   13     FORMAT(F10.5)
        ENDDO
      ENDIF
      IF(IER.EQ.0)CALL EZ_GET_CHARS('FILTERS',NFILTER,FILTER,JER)
      IF(NFILTER.GT.NFILTMAX)CALL ERRMSG('MAKE_NT_RCP','MAKE_NT',
     &  'too many filter names','F')
      IF(IER.NE.0)CALL ERRMSG('MAKE_NT_RCP','MAKE_NT',
     &  'error getting RCP parameters','F')
      CALL EZRSET
C
C **** get logical units
C
      CALL GTUNIT(9191,LIN,IER)               ! input file
      IF(IER.EQ.0)CALL GTUNIT(9191,LOUT,IER)  ! output file
      IF(IER.NE.0)CALL ERRMSG('no unit','MAKE_NT',
     &  'error getting logical unit number','F')
C
C ****  Loop over all input files
C
      DO K=1,IFILE
C
C ****  Open the next input file
C
        CHOPT = 'XI'
        CALL XCHKER(LIN,INFILE(K),XMODE,LREC,OK)
        IF(.NOT. XMODE)THEN
          CHOPT = 'IU'
          ILEN  = LREC
        ENDIF
        CALL D0OPEN(LIN,INFILE(K),CHOPT,OK)
        IF(.NOT.OK)CALL ERRMSG('D0OPEN','MAKE_NT',
     &    'error opening'//INFILE(K),'F')
        IF(XMODE)CALL XZRECL(ILEN,CHOPT)
        CALL FZFILE(LIN,ILEN,CHOPT)
        CALL INTMSG(' opened input file '//INFILE(K))
C
C... read first record
C
        IXWIPE = IXCOM + IXMAIN
        CALL MZWIPE(IXWIPE)
        CALL FZIN(LIN,IXMAIN,LHEAD,1,' ',0,0)
        IF (IQUEST(1) .LT. 0)THEN
          WRITE (MSG,10) 'IQUEST(1) =',IQUEST(1)
   10     FORMAT(A11,I4)
          CALL ERRMSG('FZIN','MAKE_NT',MSG,'F')
        ENDIF
C
        NDIM=MAXDIM
        CALL GET_TAGS(NAMES,NDIM)
        IF(NDIM.GT.512)THEN
          CALL ERRMSG('>512 tags','MAKE_NT','truncating','W')
          NDIM=512
        ENDIF
        PRINT *
        IF(K.EQ.1)THEN
C
C ****  Open the output ntuple
C
          CALL HROPEN(LOUT,'OUTPUT',OUTFILE,'N',8191,IER)
          IF(IER.NE.0)STOP 'Could not open the output file '
          CALL INTMSG(' opened output file '//OUTFILE)
          CALL BOOK_NT(NTID,NAMES,NDIM)
C
C ****  Loop over all cuts, and search for variable names in tags array
C
          DO I = 1, NCUT
            INDX(I) = GET_INDEX(NDIM, NAMES, CUTVAR(I))
            IF ( INDX(I) .LE. 0 ) THEN
              CALL ERRMSG('MAKE_NT_RCP','MAKE_NT',
     &          'cut variable '//CUTVAR(I)//' not in n-tuple','W')
              CUTVAR(I)=' '
            ENDIF                              ! if indx(i) .le. 0
          ENDDO                             ! i = 1, ncut
        ENDIF
C
C **** extract some pointers
C
        PRUN  = GET_INDEX(NDIM, NAMES, 'RUN')
        PFILT = GET_INDEX(NDIM, NAMES, 'L2BIT0_')
C
C ****  Loop over all events in the input file and fill new ntuple
C
        IF(NENTRIES.GT.100000)THEN
          IMOD=10000
        ELSEIF(NENTRIES.GT.10000)THEN
          IMOD=1000
        ELSE
          IMOD=100
        ENDIF
        IENTFILE=0
        I=0
        IER=0
        DO WHILE(IER.GE.0)
C
C... read the next record (skip if first record was not a b-o-r record)
C
          IF(IENTFILE.GT.0.OR.MOD(IQ(LHEAD+1),1000).EQ.1)THEN
            IXWIPE = IXCOM + IXMAIN
            CALL MZWIPE(IXWIPE)
            CALL FZIN(LIN,IXMAIN,LHEAD,1,' ',0,0)
            IF (IQUEST(1) .LT. 0) IER = -2
            IF (IQUEST(1) .GT. 1) IER = -1  ! no more entries
            IF(IER.NE.0)GOTO 100            ! skip the entry if error
            LUDST=GZUDST()
            IF(LUDST.LE.0)GOTO 100          ! skip the entry if no UDST bank
          ENDIF
C
          CALL GET_EVENT(XDATA,NDIM) 
          I=I+1                
          IENT=IENT+1         ! count total number of records read
          IENTFILE=IENTFILE+1 ! count number of records read from this file
          DO J = 1, NCUT
            IF(CUTVAR(J).NE.' ')THEN
              IF (RELATION(J) .EQ. '<' ) THEN
                IF (.NOT.( XDATA(INDX(J)) .LT. CUTVALUE(J)  )) GOTO 100
              ELSE IF (RELATION(J) .EQ. '=' ) THEN
                IF (.NOT.( XDATA(INDX(J)) .EQ. CUTVALUE(J) )) GOTO 100
              ELSE IF (RELATION(j) .EQ. '>' ) THEN
                IF (.NOT.( XDATA(INDX(J)) .GE. CUTVALUE(J) )) GOTO 100
              ENDIF
            ENDIF
          ENDDO                         ! loop over ncut
          IF(NFILTER.GT.0)THEN
            IRUN=IFIX(XDATA(PRUN))
            DO J=1,4
              FILTERWORDS(J)=IFIX(XDATA(PFILT-1+J))
            ENDDO
            DO J=1,NFILTER
              IF(FILTSET(FILTER(J),IRUN,FILTERWORDS))GOTO 90
            ENDDO
            GOTO 100
          ENDIF
C
   90     CALL FILL_NT(XDATA,NDIM,PASS)
          IF(PASS)THEN
            NACC=NACC+1
          ENDIF                                 
  100     CONTINUE                      ! jump here if the entry is rejected
          IF(IENT.GT.0.AND.MOD(IENT,IMOD).EQ.0)PRINT 2,IENT,
     &        ' entries processed',NACC,' accepted'
    2     FORMAT('+',I12,A18,I12,A)
          IF(NACC.GE.NACCMAX.AND.NACCMAX.GT.0)IER=-2 ! quit if enough events
        ENDDO                           ! i = 1, nentries
C
C ****  Close the input file - go to next input file
C
  101   PRINT *
        PRINT 3,I,' entries found in ntuple'
        PRINT *
    3   FORMAT('+',I12,A44,I4)
        PRINT 2,IENT,' entries processed',NACC,' accepted'
        CALL FZENDI(LIN,'TX')
        CALL D0CLOSE(LIN,' ',OK)
        IF(IER.LE.-2)GOTO 999
      ENDDO
  999 CALL HCDIR('//OUTPUT',' ')
      CALL HROUT(NTID,ICYCLE,' ')
      CALL HREND('OUTPUT')
      CALL RLUNIT(9191,LIN,IER)               ! input file
      IF(IER.EQ.0)CALL RLUNIT(9191,LOUT,IER)  ! output file
      END
