      SUBROUTINE D0DBL3_INIT(DBFILE1,IOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : DBL3 initialization
C-
C-   Inputs  :  DBFILE1(*) - DBL3 data base names.
C-
C-   Outputs :  IOK    - .TRUE. if no error
C-   Controls:
C-
C-   Created   17-JUN-1992   S. Abachi
C-   Modified  11-SEP-1992   S. Abachi Modified for unix
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) DBFILE1(*)
      INCLUDE 'D0$INC:D0DBL3_SRVR.INC'
      INCLUDE 'D0$INC:D0DBL3_LNK.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:QUEST.INC'
      CHARACTER*120 MSG_STRING
      CHARACTER*80 DBFILE(2)
      CHARACTER*80 DBFILE2(2)
      CHARACTER*3 CHOP
      CHARACTER*1 COPT
      LOGICAL IOK,FIRST,STAT,LIB$FIND_FILE
      INTEGER L,IOS,I,DOLL,IENT,J,ERR,NDET,NIOS,CLEN,IER
      INTEGER CONT,MRK,CL1(2),TRULEN,NF
      DATA IENT,FIRST/0,.TRUE./
C----------------------------------------------------------------------
C
      IF(FIRST) CALL D0DBL3_READPAR(IER)
      IF(IER .GT. 0) THEN
        CALL INTMSG(
     &' d0dbl3_init:, Problem reading parameter file. Not initialized.')
        GOTO 999
      ENDIF
      NF = 1
      IF(ICOMP .GT. 0) NF = 2
      DO I=1,NF
        DBFILE(I) = DBFILE1(I)
        DBFILE2(I) = ' '
      ENDDO
      CHOP ='SU'
      DBINI = .FALSE.
      OPTJ = .FALSE.
      IOK = .FALSE.
      CONT = 0
      SRVRNM = 'UNK'
C
      DO I=1,NF
        CL1(I) = TRULEN(DBFILE(I))
      ENDDO
C&IF VAXVMS
      DO I=1,NF
        CALL UPCASE(DBFILE(I), DBFILE(I))
      ENDDO
C&ELSE
C&ENDIF
      DO I=1,NF
        STAT = LIB$FIND_FILE(DBFILE(I)(1:CL1(I)),DBFILE(I),CONT)
        IF(.NOT. STAT) THEN
          GOTO 999
        ELSE
C&IF VAXVMS
          MRK = INDEX(DBFILE(I), ']')
          IF(MRK .GT. 0) THEN
            DBFILE2(I) = DBFILE(I)(MRK+1:)
            MRK = INDEX(DBFILE2(I), '$')
          ENDIF
C&ELSE
C&          DBFILE2(I) = DBFILE(I)
C&50        CONTINUE
C&          MRK = INDEX(DBFILE2(I), '/')
C&          IF(MRK.EQ.0)GO TO 51
C&          DBFILE2(I) = DBFILE2(I)(MRK+1:)
C&          GO TO 50
C&51        CONTINUE
C&          MRK = INDEX(DBFILE2(I), '$')
C&ENDIF
        ENDIF
C
        IF(I .EQ. 1 .AND. MRK .GT. 0) SRVRNM = DBFILE2(I)(MRK+1:MRK+3)
      ENDDO
C
      CALL LIB$FIND_FILE_END(CONT)
C
      IF(FIRST) THEN
        FIRST = .FALSE.
        DO I=1,NF
          CALL GTUNIT(171,DBLUN(I),IER)
        ENDDO
        CALL MZEBRA(0)
        CALL INZSTP
        CALL CONSTP
        CALL MZBOOK(IDVSTP,L,L,2,'$RZ$',0,0,25000,2,-1)
        CALL MZDROP(IDVSTP,L,' ')
        CALL D0DBL3_LINK
      ENDIF
C
   20 CONTINUE
C
      DO I=1,NF
        OPEN(UNIT=DBLUN(I),FILE=DBFILE(I),STATUS='OLD',SHARED,
     &       ACCESS='DIRECT',FORM='UNFORMATTED',RECL=1024,IOSTAT=IOS)
C
  900   IF(IOS.NE.0)THEN
          IF(IOS .EQ. 30) THEN
            NIOS = NIOS + 1
            IF(NIOS .LT. 2) THEN
              WRITE(MSG_STRING,94) DBFILE(I),IOS
   94         FORMAT(' File ',A40, ' could not be accessed.'
     &                        ' Will try again. IOSTAT = ',I3)
              CALL INTMSG(MSG_STRING)
              GOTO 20
            ENDIF
          ENDIF
          WRITE(MSG_STRING,95)DBFILE(I),IOS
   95     FORMAT(' Error opening file ',A40,'  IOSTAT = ',I3)
          CALL ERRDB(MSG_STRING)
          CALL INTMSG(MSG_STRING)
          IOK = .FALSE.
          GOTO 999
        ENDIF
C
        CALL DBINIT(IDVSTP,DBLUN(I),TOPN(I),LTDIR,0,CHOP)
        IF (IQUEST(1).NE.0) THEN
          CALL ERRDB('DBINIT')
          IOK = .FALSE.
          CALL DBEND
          CLOSE(UNIT=DBLUN(I))
          GO TO 999
        ENDIF
C
        CALL RZLDIR('//',' ')
        CALL DBLOGL(DBLUN(I),0)
      ENDDO
C
      DBINI = .TRUE.              ! Must call DBEND and set true
      IOK = .TRUE.
C
  999 RETURN
      END
