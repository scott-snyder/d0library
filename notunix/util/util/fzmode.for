      PROGRAM FZMODE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return FZ file mode (EXCHANGE or NATIVE)
C-   in the logical FZ$MODE and if an output file is specified convert
C-   the native input file to exchange mode.
C-
C-      $ FZMODE InFile [OutFile]
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  28-JAN-1993   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:QUEST.INC'
      INTEGER LUN, ISTAT
      PARAMETER( LUN = 80 )
      INTEGER LREC, LIN, LOUT, LSTRING,LSUB(10),NSUB,INUNIT,IOS
C
      LOGICAL XMODE, OK, THERE_IS_MORE, RZMODE
C
      CHARACTER*131 SUBSTR(10)
      CHARACTER*131 INFILE, OUTFILE
      CHARACTER*511 STRING
      CHARACTER*8 TOPDIR
C----------------------------------------------------------------------
C
C  ZEBCOM is the main zebra common block for event data storage
C
      INTEGER NNQ,NREF
      PARAMETER (NNQ=3000000)
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
C
C----------------------------------------------------------------------
C
C ****  Get command line
C
      CALL LIB$GET_FOREIGN(STRING,'FileName> ',LSTRING)
      IF ( LSTRING .LE. 0 ) THEN
        STOP 'Bye!'
      ENDIF
C
C ****  Get filename(s)
C
      CALL CHOP(STRING,SUBSTR,LSUB,NSUB)
      INFILE = SUBSTR(1)(1:LSUB(1))
      IF ( NSUB .GT. 1 ) THEN
        OUTFILE = SUBSTR(2)(1:LSUB(2))
      ELSE
        OUTFILE = ' '
      ENDIF
      LIN  = LSUB(1)
      LOUT = LSUB(2)
C
C ****  Check mode
C
      RZMODE = .FALSE.
      CALL XCHKER(LUN,INFILE,XMODE,LREC,OK)
C
C ****  Set FZ$MODE logical
C
      CALL LIB$SET_LOGICAL('FZ$MODE',' ')
      IF ( OK ) THEN
        IF ( XMODE ) THEN
          CALL LIB$SET_LOGICAL('FZ$MODE','EXCHANGE')
          PRINT*,' FZMODE --> EXCHANGE <-- ',INFILE(1:LIN)
        ELSE
          CALL LIB$SET_LOGICAL('FZ$MODE','NATIVE')
          PRINT*,' FZMODE --> NATIVE <-- ',INFILE(1:LIN)
        ENDIF
      ELSE
C
C ****  XCHKER failed.  Try opening the file as an RZ file (RZOPEN/RZFILE).
C
        LREC = 0
        CALL RZOPEN(LUN, TOPDIR, INFILE, ' ', LREC, ISTAT)
        IF(ISTAT.EQ.0)THEN
          XMODE = IQUEST(12).NE.0
          RZMODE = .TRUE.
          IF(XMODE)THEN
            CALL LIB$SET_LOGICAL('FZ$MODE','RZ_EXCHANGE')
            PRINT*,' FZMODE --> RZ_EXCHANGE <-- ',INFILE(1:LIN)
          ELSE
            CALL LIB$SET_LOGICAL('FZ$MODE','RZ_NATIVE')
            PRINT*,' FZMODE --> RZ_NATIVE <-- ',INFILE(1:LIN)
          ENDIF
        ELSE
          PRINT*,' FZMODE --> Unable to access file ',INFILE
          STOP ' '
        ENDIF
      ENDIF
C
C ****  Check whether to convert file
C
      IF ( RZMODE .OR. XMODE .OR. OUTFILE(1:1) .EQ. ' ' )  THEN
        STOP ' '
      ENDIF
C
C ****  CONVERT FROM NATIVE TO EXCHANGE
C
      PRINT*,' FZMODE --> Converting file to EXCHANGE mode'
C
      CALL LIB$SET_LOGICAL('FOR003','NL:')  ! Send crap to null
      CALL INZBRA                           ! Initialize ZEBRA
      CALL INZCOM(2)                        ! Initialize /ZEBCOM/
C
C ****  Open input file
C
      CALL EVOPIN(INFILE,' ',INUNIT,OK)
      IF ( .NOT. OK ) THEN
        PRINT*,' FZMODE --> Unable to open input file ', INFILE(1:LIN)
        STOP ' '
      ENDIF
C
      CALL EVOPWO('BOO',OUTFILE,'X',OK)
      IF ( .NOT. OK ) THEN
C
C ****  Close input file
C
        CALL FZENDI (INUNIT,'TU')
        CLOSE (UNIT=INUNIT)
        PRINT*,
     &    ' FZMODE --> Unable to open output file ', OUTFILE(1:LOUT)
        STOP ' '
      ENDIF
C
      THERE_IS_MORE = .TRUE.
      DO WHILE ( THERE_IS_MORE )
C
        CALL EVTIN(INUNIT,IOS)
        THERE_IS_MORE = (IOS.GE.0) .AND. (IOS.LE.3)
C
        IF ( THERE_IS_MORE ) THEN
          CALL EVTWOS
        ENDIF
      ENDDO
C
C ****  Close input file
C
      CALL FZENDI (INUNIT,'TU')
      CLOSE (UNIT=INUNIT)
C
C ****  Close output file
C
      CALL EVCLWO('BOO')
C
C ****  Check code
C
      IF ( IOS .LT. 0 ) THEN
        PRINT*,' FZMODE --> ** ERROR ** EVOPIN return code ', IOS
      ENDIF
C
      STOP ' '
      END
