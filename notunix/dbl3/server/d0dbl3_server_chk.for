      SUBROUTINE D0DBL3_SERVER_CHK(IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check if everything is OK for server to start
C-                         and also do necessary cleanup.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls: IER  If not 0 then trouble
C-
C-   Created  21-JUN-1992   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IER
      INCLUDE 'D0$INC:D0DBL3_SRVR.INC'
      INTEGER LU,IERR,I,LIB$DATE_TIME,ND,CL,LEN
      CHARACTER*23 DB_DATETIME
C&IF VAXVMS
      INCLUDE       '($CLIDEF)'
C&ELSE
C&
C&ENDIF
      PARAMETER (ND=10)
      CHARACTER*80 PAR(ND),COMMAND
      CHARACTER*2 SNAME
      LOGICAL STATUS,LIB$SPAWN
      CHARACTER*3 SRVRDIR
      CHARACTER*70 CMAIL3
C
      STATUS = .TRUE.
      IER = 0
      CMAIL3 = ' '
      SRVRDIR = SRVRNM
      IF(SRVRNM(1:3) .EQ. 'GLB' .OR. SRVRNM(1:3) .EQ. 'glb')
     &     SRVRDIR = 'DBM'
      CALL CUTOL(SRVRDIR)
      CMAIL3 = '/usr/sbin/Mail dbl3mail < /dbl3/'
     &            //SRVRDIR(1:3)// '/server/CHKCLN.COM'
      IF(SRVRNM .EQ. ' ' .OR. SRVRNM .EQ. 'UNK' .OR. SRVRNM .EQ. 'unk')
     &    CMAIL3 = '/usr/sbin/Mail dbl3mail < /dbl3/CHKCLN.COM'
C
      IF(ISTOP .GT. 0) THEN
        PRINT *, 'ISTOP flag was on. Will set to off.'
        CALL GTUNIT(171,LU,IERR)
        OPEN (UNIT=LU,FILE='DBPARAM',ACCESS='SEQUENTIAL'
     +               ,FORM='FORMATTED',STATUS='OLD')
        DO I=1,ND
          READ(LU,10) PAR(I)
        ENDDO
        PAR(1)(1:2) = '0 '
        REWIND(UNIT=LU)
        DO I=1,ND
          WRITE(LU,10) PAR(I)
        ENDDO
        CLOSE(UNIT=LU)
        ISTOP = 0
        CALL RLUNIT(171,LU,IERR)
   10   FORMAT(A)
      ELSEIF(ISTOP .LT. 0) THEN
        PRINT *, 'Setting of ISTOP flag not permitted since ISTOP = ',
     &     ISTOP, 'Double check everything and then set by hand.'
        IER = 1
        GOTO 999
      ENDIF
C
      IF(ISHIP .GT. 0) THEN
C&IF VAXVMS
        SNAME = SRVRNM(1:2)
        IF(SRVRNM .EQ. 'GLB') SNAME = 'DM'
        COMMAND = '@'//'CHKCLN_COM'//' '//SNAME
        CL = LEN(COMMAND)
        STATUS = LIB$SPAWN (COMMAND(1:CL)
     &                    ,,'CHKCLN_LOG'
     &                    ,,SRVRNM//'_CHKCLN_PROC')
        IF(.NOT. STATUS) THEN
          IER = 1
          CALL LIB$STOP(%VAL(STATUS))
        ELSE
          IER = 0
        ENDIF
C&ELSE
C&          CALL SYSTEM(CMAIL3)
C&ENDIF
      ENDIF
C
  998 CALL LIB$DATE_TIME(DB_DATETIME)
      PRINT *,'Server- back from cleanup on ', db_datetime
C----------------------------------------------------------------------
  999 RETURN
      END
