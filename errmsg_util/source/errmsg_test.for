      PROGRAM TEST
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To test the message handling routines.
C-
C-   Inputs  : None.
C-   Outputs : On request.
C-
C-   Created  18-DEC-1988   Jun-Jang Jeng (MSU)
C-   Updated  11-JUL-1990   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*32 IDSTRG, SUB
      CHARACTER*132 VAR
      CHARACTER*5  SEV, ANS, ANSW
      CHARACTER*20  INFILE, OUTFIL
      INTEGER INPU,OUTPU,LU,TIME,MXWN,MXLG,IREP,NREP
      LOGICAL WRN, SCRN
      INCLUDE 'D0$ERRMSG_UTIL$PARAMS:ERRMSG.PARAMS'
      INCLUDE 'D0$INC:ERRCTL.INC'
      INCLUDE 'D0$INC:ERRCNT.INC'
      INCLUDE 'D0$INC:ERRIDS.INC'
C---------------------------------------------------------------------
C  TEST ERRINI()
C---------------------------------------------------------------------
C      CALL SETCHK
C      CALL SPLTIT
      CALL INZBRA !zebra init for errmax_rcp
      CALL OUT
    5 WRITE (6, *) 'Do you want to change default situations ?(Y/N) '
      READ(5,FMT=901) ANSW
      CALL UPCASE( ANSW, ANS )
      IF ( ANS .EQ. 'Y') THEN
        WRITE(6,*) 'LOGGING UNIT NUMBER? (0)'
        READ(5,FMT=905) LU
        WRITE(6,*)' DISPLAY WARNING MESSAGES ?(Y/N) '
        READ(5,FMT=901) ANSW
        CALL UPCASE( ANSW, ANS )
        IF ( ( ANS .EQ. 'Y') .OR. ( ANS .EQ. 'T') ) THEN
          WRN = .TRUE.
        ELSE
          WRN = .FALSE.
        ENDIF
        CALL ERRINI(LU,WRN)
        CALL OUT
      ENDIF
C--------------------------------------------------------------------
C  TEST ERRMAX()
C--------------------------------------------------------------------
      WRITE(6,*) 'Do you want to set the maximum numbers of logging and
     &  warning messages ?(Y/N) '
      READ(5,901) ANSW
      CALL UPCASE( ANSW, ANS )
      IF ( ANS .EQ. 'Y') THEN
        WRITE(6,*) 'Input the key -->'
        READ(5,FMT=903) IDSTRG
        WRITE(6,*) 'Input maximum number of logging message ? '
        READ(5,FMT='(I3)') MXLG
        WRITE(6,*) 'Input maximum number of warning message ? '
        READ(5,FMT='(I3)') MXWN
        CALL ERRMAX(IDSTRG,MXLG,MXWN)
        CALL OUT
      ENDIF
C--------------------------------------------------------------------
C test ERRMAX_RCP
C--------------------------------------------------------------------
      CALL ERRMAX_RCP
C--------------------------------------------------------------------
C  TEST ERRSON, ERRSGT,  and ERRSOF
C--------------------------------------------------------------------
      WRITE(6,*)
     &  'Do you want to change handling of screen-only messages ?(Y/N) '
      READ(5,901) ANSW
      CALL UPCASE( ANSW, ANS )
      IF ( ANS .EQ. 'Y') THEN
        CALL ERRSGT(SCRN)
        WRITE(6,*) 'Presently SCREEN is ',SCRN,' Input new value-->'
        READ(5,*) SCRN
        IF (SCRN) THEN
          CALL ERRSON
        ELSE
          CALL ERRSOF
        ENDIF
        CALL OUT
      ENDIF
C---------------------------------------------------------------------
C TEST ERRMSG,ERRFND,ERRINS,ERRSUM,ERRLOG,ERRWRN,ERRHAN,ERRFAT
C---------------------------------------------------------------------
   10 WRITE(6,*) 'Interactive input ? (Y/N)'
      READ(5,FMT='(A1)') ANSW
      CALL UPCASE( ANSW, ANS )
      IF ( ANS .EQ. 'Y' ) THEN
        INPU = 5
      ELSEIF ( ANS .EQ. 'N' ) THEN
C...I believe that file input does not work......................
        WRITE(6,*) 'What is input file ?'
        READ(5, FMT='(A10)') INFILE
        WRITE(6,*) 'What is output file ?'
        READ(5,FMT='(A10)') OUTFIL
        INPU = 70
        OPEN(INPU,FILE=INFILE,STATUS = 'UNKNOWN')
        REWIND(INPU)
        OUTPU = 71
        OPEN(OUTPU,FILE=OUTFIL,STATUS = 'UNKNOWN')
        REWIND(OUTPU)
      ELSEIF ( (ANS .NE. 'Y') .AND. (ANS .NE. 'N') ) THEN
        GO TO 10
      ENDIF
C---------------------------------------------------------------------
   20 IF (INPU .EQ. 5) THEN
        WRITE(6,*) '*** IDSTRG ? '
      ENDIF
      READ(UNIT=INPU,FMT='(A32)') IDSTRG
      IF (INPU .EQ. 5 ) THEN
        WRITE(6,*) '*** SUBRID ?'
      ENDIF
      READ(UNIT=INPU,FMT='(A32)') SUB
      IF (INPU .EQ. 5 ) THEN
        WRITE(6,*) '*** VARSTR ?'
      ENDIF
      READ (UNIT=INPU, FMT='(A132)') VAR
      IF (INPU .EQ. 5 ) THEN
        WRITE(6,*) '*** SEVRTY ?'
      ENDIF
      READ(UNIT=INPU,FMT='(A5)') SEV
      IF ( INPU .NE. 5 ) THEN
        WRN = .TRUE.
        LULOG = OUTPU
      ENDIF
      IF (INPU .EQ. 5) THEN
        WRITE(6,*) '*** NUMBER OF REPEATS ? (1)'
      ENDIF
      READ(UNIT=INPU,FMT='(I3)') NREP
      IF (NREP.LE.0) NREP = 1
      DO 6 IREP = 1,NREP
      CALL ERRMSG(IDSTRG,SUB,VAR,SEV)
    6 CONTINUE
      WRITE(6,900) NENTRY
      WRITE(6,*) 'More input ? (Y/N) '
      READ(5,FMT='(A1)') ANSW
      CALL UPCASE( ANSW, ANS )
      IF( ANS .EQ. 'Y') THEN
        GO TO 20
      ENDIF
  500 CALL OUT
C----------------------------------------------------------------------
C   TEST ERRGET()
C----------------------------------------------------------------------
  600 WRITE(6,*) 'Do you want to know the number of counts of some key
     &  ?(Y/N)'
      READ (5,FMT=901) ANSW
      CALL UPCASE( ANSW, ANS )
      IF (ANS .EQ. 'Y') THEN
        WRITE(6,*) 'Input the key -->'
        READ(5, FMT=903) IDSTRG
        CALL ERRGET(IDSTRG,TIME)
        WRITE(6,FMT=904) TIME
        CALL OUT
      ELSE
        GO TO 700
      ENDIF
      GO TO 600
C-----------------------------------------------------------------------
C   TEST ERRCLR()
C-----------------------------------------------------------------------
  700 WRITE (6,*) 'Do you want to clear some key?(Y/N)'
      READ (5, FMT=901) ANSW
      CALL UPCASE( ANSW, ANS )
      IF ( ANS .EQ. 'Y') THEN
        WRITE(6,*) 'Input the key -->'
        READ(5,FMT=903) IDSTRG
        CALL ERRCLR(IDSTRG)
        CALL OUT
      ELSE
        GO TO 800
      ENDIF
      GO TO 700
  800 WRITE (6,*) 'Do you want to input more data ?(Y/N) '
      READ(5,FMT=901) ANSW
      CALL UPCASE( ANSW, ANS )
      IF ( ANS .EQ. 'Y') THEN
        GO TO 5
      ENDIF
C-------------------------------------------------------------------------
C  FORMAT
C-------------------------------------------------------------------------
      WRITE(6,*) 'TEST FINISHED !'
      CALL ERRSUM(6)
  900 FORMAT(1X,'NENTRY = ',I6)
  901 FORMAT(A5)
  903 FORMAT(A32)
  904 FORMAT(1X,'The number of count = ',I3)
  905 FORMAT(I2)
      STOP
      END
C-------------------------------------------------------------------------
C **** OUTPUT SYSTEM PARAMETERS
C--------------------------------------------------------------------------
      SUBROUTINE OUT
      IMPLICIT NONE
      INTEGER POS1
      LOGICAL FND, SCRN
      CHARACTER*1 ANS,ANSW
      CHARACTER*32 IDSTRG,IDUP
      INCLUDE 'D0$ERRMSG_UTIL$PARAMS:ERRMSG.PARAMS'
      INCLUDE 'D0$INC:ERRCTL.INC'
      INCLUDE 'D0$INC:ERRCNT.INC'
      INCLUDE 'D0$INC:ERRIDS.INC'
C
      WRITE(6,*) 'Do you want to see MAXLOG and MAXWRN ?(Y/N)'
      READ(5,FMT='(A1)') ANSW
      CALL UPCASE( ANSW, ANS )
      IF ( ANS .EQ. 'Y') THEN
        WRITE(6,*) 'Input the key -->'
        READ(5,FMT='(A32)') IDSTRG
        CALL UPCASE(IDSTRG,IDUP)        ! this is not a user routine, so
                                        ! standardize the input
        CALL ERRFND(IDUP,POS1,FND)
        IF ( FND ) THEN
          WRITE(6,FMT=907) MAXLOG(POS1), MAXWRN(POS1)
        ELSE
          WRITE(6,*) ' NOT FOUND !'
        ENDIF
      ENDIF
C
      WRITE(6,*) 'Do you want to see the contents of database ?(Y/N)'
      READ(5,FMT='(A1)' ) ANSW
      CALL UPCASE( ANSW, ANS )
      IF ( ANS .EQ. 'Y' ) THEN
        CALL ERRSGT(SCRN)
        CALL ERRSUM(6)
        CALL ERRSUM(LULOG)
        WRITE(6,FMT=902) MAXSTR,OVLCNT
        WRITE(6,FMT=906) WARN, LULOG
        WRITE(6,FMT=908) SCRN
        WRITE(LULOG,FMT=902) MAXSTR,OVLCNT
        WRITE(LULOG,FMT=906) WARN, LULOG
        WRITE(LULOG,FMT=908) SCRN
      ENDIF
C
  902 FORMAT(1X,'Number of storage = ', I3,' Overflow count = ',I3)
  906 FORMAT(1X,'WARN = ',L1, '     Logging unit = ',I2)
  907 FORMAT(1X,'MAXLOG = ', I6,'      MAXWRN = ',I6)
  908 FORMAT(1X,'SCRN = ',l1)
  999 RETURN
      END
