      SUBROUTINE D0DAD_TEXT_STREAM(FEVT,CFDF,FEC,IERR)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Use a private bitmask definition to create
C-     a D0DAD file ala /mode=user and /mode=stream
C-
C-   Inputs  : FEVT   - C - Event/bitmask list file
C-             CFDF   - C - D0Dadf file name
C-             FEC    - C - Event catalog name 
C-   Outputs : IERR   - I - Error return, 0 ==> All is OK.
C-   Controls:
C-
C-   Created  28-DEC-1994   John D. Hobbs
C-   Modified 17-MAY-1995   JDH - Get around FORTRAN limit on I/O units
C-
C-------------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:d0dadcom.inc/NOLIST'
      INCLUDE 'D0$INC:d0dad.inc/NOLIST'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER NIOMAX
      PARAMETER(NIOMAX=40)
C
      CHARACTER*(*) FEVT,FEC,CFDF
      CHARACTER CINOPT*1,NCTMP*32,STRTMP*128,CDFOPT*1,INLINE*132,BITST*7
      INTEGER IFTYPE,I,IFID,IERR,NEV,NALL,IKEY,IBTYPE,J,K,L
      INTEGER IDATE(2),N,NSTREAMS,MASK(5),NBMAX,NFAIL,NEVMAX,NW
      INTEGER NLINES,NSTREAM_MAX,JSTR,JSTR_MIN,JSTR_MAX
C
      LOGICAL  LSRUN,LSEVT,SORTED,OK
      LOGICAL  D0DAD_RE_RANGE
      INTEGER  LENOCC,JBIT
C
      IF( LDDBG.GT.5 ) THEN
         WRITE(*,9901) FEVT(1:LENOCC(FEVT)),
     +       CFDF(1:LENOCC(cFDF)),FEC(1:LENOCC(FEC))
 9901    FORMAT('  ** Entering D0DAD_TEXT_STREAM ***',/,
     +          '      Input event list/bit mask file: ',A,/,
     +          '      Output D0DAD filelist: ',A,/,
     +          '      Input event catalog: ',A,/)
         IF( ISELR(1).NE.0 ) WRITE(*,9904) 'Run',ISELR(1),ISELR(2)
         IF( ISELE(1).NE.0 ) WRITE(*,9904) 'Event',ISELE(1),ISELE(2)
 9904    FORMAT('      ',A,' range from ',I8,' to ',I10)
         WRITE(*,9903)
 9903    FORMAT(/,/)
      ENDIF
C
C  Open input event catalog...
C
      CALL D0DAD_OPEN(JFEC,FEC,'R',IDADEC,IERR)
      IF( IERR.NE.0 ) THEN
         IF( LDDBG.GT.0 ) WRITE(*,9001) IERR,'EC'
 9001    FORMAT(' D0DAD_STREAM: Error ',I5,' from D0DAD_OPEN(',A2,')')
         IERR = -1
         GOTO 998
      ENDIF
C
C  See if it's possible to read the entire input file into memory.  If
C  so, do it, sort, and set the sorted flag...  Set an upper bound on
C  the number of events in a single file by assuming up to NSMAX streaming
C  bits.  The words/event is then 1(run#)+1(event#)+NSMAX/32 = 7, or 32 
C  bytes/event.  Thus, (eg) 100k events requires 700K words of storage...
C
C  NB: The protected link LUEHD is used here.  If an UE file is opened
C   simultaneously with this routine (How, I don't know?) it will cause
C   problems.
C
      CALL D0OPEN(IDADOK,FEVT(1:LENOCC(FEVT)),'IF',OK)
      IF( .NOT.OK ) GOTO 902
      NEVMAX=100000
      N=0
      NBMAX=0
      SORTED=.FALSE.
 50   CONTINUE
        READ(IDADOK,'(A)',END=60,ERR=903) INLINE
        CALL GETWORD(INLINE,1,STRTMP)
        READ(STRTMP,*,err=903) IDRUN
        CALL GETWORD(INLINE,2,STRTMP)
        READ(STRTMP,*,ERR=903) IDEVNT
        CALL GETWORD(INLINE,3,STRTMP)
        N=N+1
        NBMAX=MAX(NBMAX,LENOCC(STRTMP)/8)
      GOTO 50
C
 60   CONTINUE
      IF( N.LE.NEVMAX .AND. NBMAX.LE.5 ) THEN
        SORTED=.TRUE.
        REWIND(IDADOK)
        NW=(2+NBMAX)*N
        CALL MZBOOK(IXDDAD,LUEHD,0,2,'TEMP',0,0,NW,2,0)
        IF( LUEHD.LE.0 ) THEN
          IERR=-4
          GOTO 998
        ENDIF
        DO I=1,N
          J=(2+NBMAX)*(I-1)
          READ(IDADOK,'(A)',ERR=905) INLINE
          CALL GETWORD(INLINE,1,STRTMP)
          READ(STRTMP,*,err=905) IQ(LUEHD+J+1)
          CALL GETWORD(INLINE,2,STRTMP)
          READ(STRTMP,*,ERR=905) IQ(LUEHD+J+2)
          CALL GETWORD(INLINE,3,STRTMP)
          DO WHILE( (LENOCC(STRTMP)/8).LT.NBMAX )
            STRTMP='0'//STRTMP
          ENDDO
          DO K=1,NBMAX
            L=8*(NBMAX-K)
            READ(STRTMP(L+1:L+8),'(Z8.8)') IQ(LUEHD+J+2+K)
          ENDDO
        ENDDO
CCC        CALL SORT_THIS_ZEBRA_BANK_NOW
CCC Process the bit mask character string into a true mask.
      ELSEIF( NBMAX.GT.5 ) THEN
        IERR = -6
        GOTO 998
      ENDIF
      CLOSE(IDADOK)
C
C  Setup the output filenames for the streams...
C
      I=0
      CDFTAG=CECTAG
      CALL D0OPEN(IDADOK,CFDF(1:LENOCC(CFDF)),'IF',OK)
      IF( .NOT.OK ) GOTO 907
 30   CONTINUE
        READ(IDADOK,'(A)',END=40,ERR=908) INLINE
        IF( I.EQ.NSMAX ) THEN
          IF(LDDBG.GT.0) WRITE(*,9913) NSMAX
 9913     FORMAT(' Maximum number of streams=',I4,'.  Exceeded')
          IERR = -13 
          GOTO 999
        ENDIF
        I=I+1
        CDFILE(I)=' '
        COMMENT(I)=' '
        CALL GETWORD(INLINE,1,CDFILE(I))
        CALL GETWORD(INLINE,2,COMMENT(I))
        IF( LENOCC(CDFILE(I)).GT.0 ) THEN
          CALL CUTOL(CDFILE(I))
          FNTMP=PREFIX(1:LENOCC(PREFIX))//CDFILE(I)
          CALL FILENAME_PARSE(FNTMP,'EXT',STRTMP,J)
          IF(J.LE.0) FNTMP=FNTMP(1:LENOCC(FNTMP))//'.d0dadf'
          SDFOPT(I)='A'
          CALL D0DAD_GETFNO(CDFILE(I),SDFOPT(I))
          CDFILE(I)=FNTMP
          NSTREAMS=NSTREAMS+1
          NSTREAM_MAX=I
        ENDIF
C  Check for duplicate trigger names (YUCK)
        DO J=1,I-1
          IF( CDFILE(I).EQ.CDFILE(J) .AND. LENOCC(CDFILE(I)).GT.0 ) THEN
            IF(LDDBG.GT.1) WRITE(D0DAD_ERRTXT,8001) I,J
 8001       FORMAT(' Duplicate d0dad file names.  Bits ',I3,' and',I3)
            CALL ERRMSG('DuplicateName','D0DAD_TEXT_STREAM',
     <         D0DAD_ERRTXT,'W')
            CALL FILENAME_PARSE(CDFILE(I),'NAM',FNTMP,K)
            WRITE(BITST,1234) I
 1234       FORMAT('_BIT',I3.3)
            FNTMP=FNTMP(1:LENOCC(FNTMP))//BITST
            CALL FILENAME_PARSE(CDFILE(I),'EXT',NCTMP,K)
            CDFILE(I)=FNTMP(1:LENOCC(FNTMP))//NCTMP(1:LENOCC(NCTMP))
          ENDIF
        ENDDO
      GOTO 30
 40   CONTINUE
      CLOSE(IDADOK)
      NLINES=I
C
C  Open dad files for each stream
C
      IF( LDDBG.GT.4 ) WRITE(*,1001)
 1001 FORMAT(/,' Defined streams:',/,
     +         '    Stream          Mask        File')
      DO I=1,NLINES
        IF(LDDBG.GT.4)WRITE(*,1002)I,CDFILE(I)(1:LENOCC(CDFILE(I))),
     +     COMMENT(I)(1:LENOCC(COMMENT(I)))
 1002   FORMAT(5X,I4,2X,' ',A50,1X,A50)
      ENDDO
      IF( LDDBG.GT.4 ) WRITE(*,1003) NSTREAMS
 1003 FORMAT(' Defined ',I4,' streams',/)
C
C  Do the streaming. This is done in chunks of NIOMAX output DF files ino
C  order to get around the fortran open file limit.  The loop on JSTR is
C  simply there to implement this hack.
C
      NEV=0
      NALL=0
      NFAIL=0
      CALL VZERO(NSEL,NSMAX)
      DO JSTR=1,((NSTREAM_MAX-1)/NIOMAX)+1
C
C    Open the D0DAD files for this round...
C
        JSTR_MIN = (JSTR-1)*NIOMAX + 1
        JSTR_MAX = MIN(NSTREAM_MAX,JSTR_MIN+NIOMAX-1)
        DO J=JSTR_MIN,JSTR_MAX
          IF( LENOCC(CDFILE(J)).GT.0 ) THEN
            CALL D0DAD_OPEN(JFDF,CDFILE(J),SDFOPT(J),ISTLUN(J),IERR)
            IF( IERR.NE.0 ) THEN
              IF( LDDBG.GT.0 ) WRITE(*,9001) IERR,'DF'
              IERR = -9
              GOTO 998
            ENDIF
          ENDIF
        ENDDO
C
C    Actually do the streaming...
C
        I=0
 10     CONTINUE
C
C  Get the run/event/mask for the next event...
C
          IF( SORTED ) THEN
            IF( I.EQ.N ) GOTO 20
            I=I+1
            J=(2+NBMAX)*(I-1)
            IDRUN=IQ(LUEHD+J+1)
            IDEVNT=IQ(LUEHD+J+2)
            CALL UCOPY(IQ(LUEHD+J+3),MASK,NBMAX)
          ELSE
C&IF IBMAIX
C&            IF( I.EQ.0 ) OPEN(IDADOK,FILE=FEVT,ERR=910)
C&
            IF( I.EQ.0 ) OPEN(IDADOK,FILE=FEVT,READONLY,ERR=910)
C&ENDIF
            READ(IDADOK,'(A)',END=20) INLINE
            CALL GETWORD(INLINE,1,STRTMP)
            READ(STRTMP,*,ERR=903) IDRUN
            CALL GETWORD(INLINE,2,STRTMP)
            READ(STRTMP,*,ERR=903) IDEVNT
            CALL GETWORD(INLINE,3,STRTMP)
            I=I+1
          ENDIF
C
C  Check run and event number range....
C
          IF( .NOT.D0DAD_RE_RANGE(ISELR,ISELE,IDRUN,IDEVNT) ) GOTO 10
          CALL ECGET(IDADEC,IDRUN,IDEVNT,IstMSK,IFID,IDZRN,IDZBO,IERR)
          IF( IERR.EQ.1 ) GOTO 20
          IF( IERR.LT.0 ) THEN
            IF( LDDBG.GT.0 ) WRITE(*,9003) IERR,IDADEC
 9003       FORMAT(' D0DAD_STREAM: Error ',I3,' from ECGET on unit',I3)
            IERR = -11
            GOTO 998
          ENDIF
C
C  Check bitmask and write to output stream if OK.
C
          IF(JSTR.EQ.1) NEV=NEV+1
          CALL ECGET_TIMESTAMP(IDADEC,IDATE)
          DO J=JSTR_MIN,JSTR_MAX
            K=(J-1)/32+1
            IF( JBIT(MASK(K),MOD(J-1,32)+1).NE.0 ) THEN
              CALL DFPUT(ISTLUN(J),IDRUN,IDEVNT,IFID,IDZRN,IDZBO,IDATE,
     >         IERR)
              IF( IERR.LT.0 ) THEN
                K=LENOCC(CDFILE(J))
                IF( LDDBG.GT.0 ) WRITE(*,9004) IERR,J,CDFILE(J)(1:K)
 9004           FORMAT(' D0DADS_TEXT_TREAM: Error',I4,
     >               ' from DFPUT, on (stream: ',I4,'):',A)
                IERR = -12
                GOTO 998
              ENDIF
              NALL=NALL+1
              NSEL(J)=NSEL(J)+1
            ENDIF
          ENDDO
          IF( JSTR.EQ.1 .AND. MOD(NEV,2500).EQ.0 .AND. LDDBG.GT.4) 
     >      WRITE(*,9902) NEV,NALL
 9902     FORMAT('    Events checked:',I8,', Selected: ',I8)
        GOTO 10
C
C    Close the d0dad files opened for this round...
C
 20     CONTINUE
        DO J=JSTR_MIN,JSTR_MAX
          IF( ISTLUN(J).GT.0 ) CALL D0DAD_CLOSE(ISTLUN(J),IERR)
        ENDDO
        IF( .NOT.SORTED ) CLOSE(IDADOK)
C
      ENDDO
C
C  This is really the end of the processing
C
      CALL D0DAD_CLOSE(IDADEC,IERR)
C
 999  CONTINUE
      IF( LDDBG.GT.4 ) THEN
        WRITE(*,9907) NEV,NALL
 9907   FORMAT('    Events checked:',I8,', Selected: ',I8,/)
        DO I=1,NSTREAM_MAX
          IF(ISTLUN(I).NE.0) WRITE(*,9906) I,NSEL(I)
 9906     FORMAT('        Stream ',I4,' contains ',I10,' events')
        ENDDO
      ENDIF
      RETURN
C
 902  CONTINUE
      CALL ECCLOS(IDADEC,IERR)
      IERR = -2
      RETURN
C
 903  CONTINUE
      CALL ECCLOS(IDADEC,IERR)
      IERR = -3
      RETURN
C
 905  CONTINUE
      CALL ECCLOS(IDADEC,IERR)
      IERR = -5
      RETURN
C
 907  CONTINUE
      CALL ECCLOS(IDADEC,IERR)
      IERR = -7
      RETURN
C
 908  CONTINUE
      CALL ECCLOS(IDADEC,IERR)
      IERR = -8
      RETURN
C
 910  CONTINUE
      CALL ECCLOS(IDADEC,IERR)
      IERR = -10
      RETURN
C
 998  CONTINUE
      RETURN
      END
