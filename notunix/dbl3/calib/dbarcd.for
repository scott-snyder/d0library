      SUBROUTINE DBARCD(DECT,DBFILE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Used for deletion part of archiving procedure.
C-
C-   Inputs  :   DECT    detector name
C-   Outputs :
C-   Controls:
C-
C-   Created  19-FEB-1991   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) DECT, DBFILE
      INCLUDE 'D0$INC:DBSTP.INC'
      INTEGER ITYP,NTYP,ICR,NCR,IRUN
      INTEGER RUN, CRATE, NRUN, NCRATE
      INTEGER LRUN(10000),LCRATE(10000)
      INTEGER UNITD,IERR,IOS
      LOGICAL IOK, MENU1, GOTOP
      CHARACTER*25 PATH,FILNAM
      CHARACTER*80 MSGSTR
      CHARACTER*40 COMAND
      CHARACTER*1 ANS,CHSF
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
        UNITD = 6
      ELSE
        CALL GTUNIT ( 171, UNITD, IERR )
        FILNAM = 'DBARCV_DELE_'//DECT(1:3)//'.LIS'
        OPEN ( UNIT=UNITD, STATUS='NEW', FORM='FORMATTED', FILE=
     &        FILNAM,
     &         IOSTAT = IOS )
        IF ( IOS .NE. 0 ) THEN
          WRITE ( MSGSTR, 1003 ) FILNAM, UNITD
 1003     FORMAT ( ' Error opening file = ', A, ' Unit =', I )
          CALL INTMSG ( MSGSTR )
          GOTO 998
        ELSE
          WRITE ( MSGSTR, 1004 ) FILNAM
 1004     FORMAT ( ' Output to file ', A )
          CALL INTMSG ( MSGSTR )
        ENDIF
      ENDIF
C
      CALL DBCLB_INITIALIZE(DBFILE,'U',IOK)
      IF(.NOT. IOK) THEN
        CALL INTMSG(' Error in initialization ')
        GOTO 998
      ENDIF
      DO ITYP=1,NTYP
        CALTYPE = CALTYP(ITYP)
        CALL DBCLB_PATH(CALTYPE,DECT(1:3),PATH)
        IF(ITYP .EQ. 1) THEN
          MENU1 = .TRUE.
        ELSE
          MENU1 = .FALSE.
        ENDIF
        CALL DBARC_LIST(MENU1,UNITD,PATH,NRUN,LRUN,LCRATE)
        DO IRUN=1,NRUN
          RUN = LRUN(IRUN)
          CRATE = LCRATE(IRUN)
          CALL DBARC_DELETE(PATH,DECT(1:3),CRATE,RUN)
        ENDDO
      ENDDO
C
  998 CONTINUE
      IF(CALL_DBEND) CALL DBCLB_FINISH
C
      IF ( CHSF .NE. 'S' ) THEN
        CLOSE (UNIT=UNITD)
        CALL RLUNIT ( 171, UNITD, IERR )
      ENDIF
C
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
