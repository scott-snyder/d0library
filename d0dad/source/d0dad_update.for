      SUBROUTINE D0DAD_UPDATE(CINNAM,CUFNAM,CFCNAM,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Update either an event catalog or a d0dad 
C-      using an unsorted event catalog or update log respectively.
C-      Called from d0dad.
C-
C-   Inputs  : CINNAM - Update file (unsorted EC or update log)
C-           : CUFNAM - File to be updated (EC or DF)
C-           : CFCNAM - File catalog (EC updating only)
C-   Outputs : 
C-   Controls: 
C-
C-   Created   8-NOV-1993   John D Hobbs
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:d0dadcom.inc/NOLIST'
      INCLUDE 'D0$INC:d0dad.inc/NOLIST'
      CHARACTER*(*) CUFNAM,CINNAM,CFCNAM
      INTEGER IUFNAM,IFTYPE,IERR,IFLAG,IFILE,NFILES,NEVTS,IFDUPL
      INTEGER ILENIN,ILENUF,ILENFC,NEMPTY
      LOGICAL LNEWFILE
      INTEGER LENOCC
      LOGICAL D0DAD_RE_RANGE
      EXTERNAL LENOCC,D0DAD_RE_RANGE
C----------------------------------------------------------------------
C
      CALL D0DAD_FTYPE(CINNAM,IFTYPE)
      CALL D0DAD_CPAD(CINNAM)
      CALL D0DAD_CPAD(CUFNAM)
      CALL D0DAD_CPAD(CFCNAM)
      ILENIN=MIN(88,LENOCC(CINNAM))
      ILENUF=MIN(88,LENOCC(CUFNAM))
      ILENFC=MIN(88,LENOCC(CFCNAM))
*
*  Update event catalog...
*
      IF( IFTYPE.EQ.JFUE ) THEN

         IF( LDDBG.GT.5 ) WRITE(*,1001) CINNAM(1:ILENIN),
     +     CUFNAM(1:ILENUF),CFCNAM(1:ILENFC)
 1001    FORMAT(/,'  **** In D0DAD_UPDATE ****',/,
     +         '     Unsorted event catalog: ',A,/,
     +         '     Event catalog: ',A,/,
     +         '     File catalog: ',A,/)
*
*     Open the input unsorted event catalog...
*
         CALL D0DAD_OPEN(JFUE,CINNAM,'R',IDADUE,IERR)
         IF( IERR.NE.0 ) THEN
            IF( LDDBG.GT.0 ) WRITE(*,9001) IERR
 9001       FORMAT(' D0DAD_UPDATE: Return from D0DAD_OPEN(ue)=',I5)
            IERR = -2
            GOTO 999
         ENDIF
*
*     Open the event catalog...
*
         CALL D0DAD_OPEN(JFEC,CUFNAM,'A',IDADEC,IERR)
         IF( IERR.NE.0 ) THEN
            IF( LDDBG.GT.0 ) WRITE(*,9002) IERR
 9002       FORMAT(' D0DAD_UPDATE: Return from D0DAD_OPEN(ec)=',I5)
            IERR = -3
            GOTO 999
         ENDIF
*
*     Open the file catalog and get current file count...
*
         CALL D0DAD_OPEN(JFFC,CFCNAM,'A',IDADFC,IERR)
         IF( IERR.NE.0 ) THEN
            IF( LDDBG.GT.0 ) WRITE(*,9003) IERR
 9003       FORMAT(' D0DAD_UPDATE: Return from D0DAD_OPEN(fc)=',I5)
            IERR = -4
            GOTO 999
         ENDIF
         CALL FCHRD(IDADFC,IFILE,IERR)
*
*     Set timestamp if needed...
*
       IF(DATTIM_STAMP(1).NE.0)CALL ECSET_TIMESTAMP(DATTIM_STAMP,.TRUE.)
*
*     Process...
*
 10      CONTINUE
            CALL UEGET(IDADUE,IDRUN,IDEVNT,ISTMSK,IDZRN,IDZBO,CFNAME,
     +          CGNAME,CTAPE,CFCCOM,IERR)
            IF( IERR.GT.0 ) GOTO 20
            IF( IERR.LT.0 ) THEN
               IF( LDDBG.GT.0 ) WRITE(*,9004) IERR
 9004          FORMAT(' D0DAD_UPDATE: Return from UEGET=',I4)
               IERR = -5
               GOTO 999
            ENDIF
            IF( IDRUN.EQ.-1 ) THEN
               IF( LNEWFILE ) NEMPTY=NEMPTY+1
               LNEWFILE=.TRUE.
               NFILES=NFILES+1
               IF( .NOT.LBLIND ) THEN
                 IFDUPL=0
                 CALL FCDUPL(IDADFC,CFNAME,IFDUPL,IERR)
                 IF( IERR.GT.0 ) CALL FCRUB(IDADFC,IFDUPL,IFILE+1,IERR)
               ENDIF
               IFILE=0
               CALL FCPUT(IDADFC,CFNAME,CGNAME,CTAPE,CFCCOM,IFILE,IERR)
               IF( IERR.NE.0 ) THEN
                 IF( LDDBG.GT.0 ) WRITE(*,9005) IERR
 9005            FORMAT(' D0DAD_UPDATE: Error ',I10,' from FCPUT')
                 IERR = -6
                 GOTO 999
               ENDIF
               IF(LDDBG.GT.4)THEN
                 CALL D0DAD_CPAD(CFNAME)
                 CALL D0DAD_CPAD(CGNAME)
                 CALL D0DAD_CPAD(CTAPE)
                 CALL D0DAD_CPAD(CFCCOM)
                 WRITE(*,1002)IFILE,IERR,
     +           CFNAME(1:LENOCC(CFNAME)),CGNAME(1:LENOCC(CGNAME)),
     +           CTAPE(1:LENOCC(CTAPE)),CFCCOM(1:LENOCC(CFCCOM))
               ENDIF
            ELSE
              IF(.NOT.D0DAD_RE_RANGE(ISELR,ISELE,IDRUN,IDEVNT)) GOTO 10
              LNEWFILE=.FALSE.
              NEVTS=NEVTS+1
              IDRUN=IDRUN+IROFF
              IDEVNT=IDEVNT+IEOFF
              CALL ECPUT(IDADEC,IDRUN,IDEVNT,ISTMSK,IFILE,IDZRN,IDZBO,
     +           1,IERR)
              IF( IERR.NE.0 ) THEN
                IF( LDDBG.GT.0) WRITE(*,9006) IERR,IDRUN,IDEVNT
 9006           FORMAT(' D0DAD_UPDATE: Error ',I6,
     +                ' from ECPUT for R/E',2I8)
                IERR = -7
                GOTO 999
              ENDIF
            ENDIF
         GOTO 10
*
*     eof...
*
 20      CONTINUE
C
C     Flush internal buffer...
C
         IDRUN=-1
         CALL ECPUT(IDADEC,IDRUN,IDEVNT,ISTMSK,IFILE,IDZRN,IDZBO,
     +           1,IERR)
C
C     And done...
C
         CALL D0DAD_CLOSE(IDADUE,IERR)
         CALL D0DAD_CLOSE(IDADEC,IERR)
         CALL D0DAD_CLOSE(IDADFC,IERR)
         IF( LDDBG.GT.3 ) WRITE(*,1003) NFILES,NEVTS,NEMPTY
         IERR = 0

      ELSE
*
         IF( LDDBG.GT.0 ) WRITE(*,9007) IFTYPE
 9007    FORMAT(' D0DAD_UPDATE: Unknown UPDATE file type: ',I4)
         IERR = -1
*
      ENDIF
C
  999 CONTINUE
      RETURN
 1002 FORMAT(' New input file. FID =',I6,', Error =',I6,/,
     +                '   File Name: ',A,/,
     +                '   Generic Name: ',A,/,
     +                '   Tape Info: ',A,/
     +                '   Comment field: "',A,'"'/)
 1003 FORMAT(/,' End of input file reached.  Updated for ',I5,' files'
     >   ,' containing ',I10,' events and ',I5,' empty files.',/)
      END
