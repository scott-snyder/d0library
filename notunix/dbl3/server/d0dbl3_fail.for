      SUBROUTINE D0DBL3_FAIL(IOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Called at failure time
C-
C-   Inputs  : *IOK*   As input: If non-zero then=value of ISTOP to set
C-
C-   Outputs : *IOK*   If not zero then trouble
C-   Controls:
C-
C-   Created  25-JUN-1992   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IOK
      INCLUDE 'D0$INC:D0DBL3_SRVR.INC'
      INCLUDE 'D0$INC:QUEST.INC'
C&IF VAXVMS
      INCLUDE       '($CLIDEF)'
C&ELSE
C&
C&ENDIF
      LOGICAL STATUS,LIB$SPAWN
      INTEGER IER,ND,CL,TRULEN
      PARAMETER (ND=10)
      INTEGER LU,IERR,I,LIB$DATE_TIME,NI
      CHARACTER*80 PAR(ND)
      CHARACTER*23 DB_DATETIME
      CHARACTER*80 COMMAND
      CHARACTER*9 PAR1
      CHARACTER*3 SRVRDIR
      CHARACTER*70 CMAIL2
C
      CMAIL2 = ' '
      SRVRDIR = SRVRNM
      IF(SRVRNM(1:3) .EQ. 'GLB' .OR. SRVRNM(1:3) .EQ. 'glb')
     &     SRVRDIR = 'DBM'
      CALL CUTOL(SRVRDIR)
      CMAIL2 = '/usr/sbin/Mail dbl3mail < /dbl3/'
     &           //SRVRDIR(1:3)// '/server/FAILED.COM'
      IF(SRVRNM .EQ. ' ' .OR. SRVRNM .EQ. 'UNK' .OR. SRVRNM .EQ. 'unk')
     &    CMAIL2 = '/usr/sbin/Mail dbl3mail < /dbl3/FAILED.COM'
C
      STATUS = .TRUE.
      ISTOP = 1
      IF(IOK .NE. 0) THEN
        ISTOP = IOK
        PRINT *, 'ISTOP flag will set to: ', IOK
        CALL GTUNIT(171,LU,IERR)
        OPEN (UNIT=LU,FILE='DBPARAM',ACCESS='SEQUENTIAL'
     +               ,FORM='FORMATTED',STATUS='OLD')
        DO I=1,ND
          READ(LU,10) PAR(I)
        ENDDO
        WRITE(PAR(1)(1:2),'(I2)') IOK
        REWIND(UNIT=LU)
        DO I=1,ND
          WRITE(LU,10) PAR(I)
        ENDDO
        CLOSE(UNIT=LU)
        CALL RLUNIT(171,LU,IERR)
   10   FORMAT(A)
      ENDIF
C
      IOK = 0
C
C&IF VAXVMS
      STATUS = LIB$SPAWN (,'FAILED_COM'
     &                    ,'FAILED_LOG'
     &                    ,CLI$M_NOWAIT
     &                    ,SRVRNM//'_FAILED_PROC')
C
      NI = 1
      IF(IABS(IQUEST(1)) .GT. 9) NI =
     &  NI + INT(ALOG10(ABS(FLOAT(IQUEST(1)))))
      IF(IQUEST(1) .LT. 0) NI = NI + 1
      WRITE(PAR1, '(I<NI>)') IQUEST(1)
      CL = TRULEN(PAR1)
      COMMAND = '@'//'DBERR_COM'//' '//PAR1(1:CL)
      CL = TRULEN(COMMAND)
      STATUS = LIB$SPAWN(COMMAND(1:CL)
     &                     ,,'DBERR_LOG'
     &                     ,CLI$M_NOWAIT
     &                     ,SRVRNM//'_ERR_PROC')
C&ELSE
C&          CALL SYSTEM(CMAIL2)
C&ENDIF
C
      CALL LIB$DATE_TIME(DB_DATETIME)
      PRINT *,'Server- This failure occured at ',db_datetime
C----------------------------------------------------------------------
  999 RETURN
      END
