      SUBROUTINE DBARCC(DECT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : used for copy part of archiving procedure.
C-
C-   Inputs  :   DECT  detector name
C-   Outputs :
C-   Controls:
C-
C-   Created  19-FEB-1991   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) DECT
      INCLUDE 'D0$INC:DBSTP.INC'
      INTEGER ITYP,NTYP,ICR,NCR,IRUN
      INTEGER RUN, CRATE, NRUN, NCRATE
      INTEGER LRUN(10000),LCRATE(10000)
      INTEGER UNITC,IERR,IOS
      LOGICAL IOK, MENU1, GOTOP
      CHARACTER*25 PATH,FILNAM
      CHARACTER*80 MSGSTR
      CHARACTER*40 COMAND
      CHARACTER*1 ANS,CHSF
      CHARACTER*48 DBFILE
      CHARACTER*17 CALTYPE, CALTYP(20)
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C
      IF(CALL_DBEND) CALL DBCLB_FINISH
      CALL DBCLB_CALTYPE(DECT, NTYP, CALTYP)
C
      CALL GETPAR ( 1, ' Dump to screen or file ? S/[F]> ', 'U',
     &      CHSF )
      IF ( CHSF .EQ.'S' ) THEN
        UNITC = 6
      ELSE
        CALL GTUNIT ( 171, UNITC, IERR )
        FILNAM = 'DBARCV_COPY_'//DECT(1:3)//'.LIS'
        OPEN ( UNIT=UNITC, STATUS='NEW', FORM='FORMATTED', FILE=
     &        FILNAM,
     &         IOSTAT = IOS )
        IF ( IOS .NE. 0 ) THEN
          WRITE ( MSGSTR, 1000 ) FILNAM, UNITC
 1000     FORMAT ( ' Error opening file = ', A, ' Unit =', I )
          CALL INTMSG ( MSGSTR )
          GOTO 998
        ELSE
          WRITE ( MSGSTR, 1001 ) FILNAM
 1001     FORMAT ( ' Output to file ', A )
          CALL INTMSG ( MSGSTR )
        ENDIF
      ENDIF
C
      DBFILE = 'DBCALIB$'//DECT(1:3)//'_S'
C
      DO ITYP=1,NTYP
        CALTYPE = CALTYP(ITYP)
        CALL DBCLB_PATH(CALTYPE,DECT(1:3),PATH)
        CALL DBCLB_INITIALIZE(DBFILE,' ',IOK)
        IF(.NOT. IOK) THEN
          CALL INTMSG(' Error in initialization ')
          GOTO 998
        ENDIF
        IF(ITYP .EQ. 1) THEN
          MENU1 = .TRUE.
        ELSE
          MENU1 = .FALSE.
        ENDIF
        CALL DBARC_LIST(MENU1,UNITC,PATH,NRUN,LRUN,LCRATE)
        CALL DBCLB_FINISH
        DO IRUN=1,NRUN
          RUN = LRUN(IRUN)
          CRATE = LCRATE(IRUN)
          CALL DBARC_UPDATE(DECT(1:3),CALTYPE,CRATE,RUN)
        ENDDO
      ENDDO
C
  998 CONTINUE
      IF(CALL_DBEND) CALL DBCLB_FINISH
C
      IF ( CHSF .NE. 'S' ) THEN
        CLOSE (UNIT=UNITC)
        CALL RLUNIT ( 171, UNITC, IERR )
      ENDIF
C
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
