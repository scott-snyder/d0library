      SUBROUTINE D0DAD_INFO(INNAME,INFO_NAME,IERR)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Write a the run number and lowest and highest
C-     event numbers contained in a single-run UE file to an output file.
C-     Also write the name in the filename field...
C-
C-   Inputs  : INNAME    - Name of input UE file
C_             INFO_NAME - Name of summary file
C-   Outputs : IERR - 0==> All is OK
C-   Controls:
C-
C-   Created   4-Sep-1994   John D. Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:d0dad.inc'
      INCLUDE 'D0$INC:d0dadcom.inc'
      CHARACTER*(*) INNAME,INFO_NAME,FILENAME*132
      INTEGER IERR
      INTEGER IR,IE,IEMIN,IEMAX,NEV
      LOGICAL OK
C- External routines
      INTEGER LENOCC
C-----------------------------------------------------------------------
C
      CALL D0DAD_OPEN(JFUE,INNAME,'R',IDADUE,IERR)
      IF( IERR.NE.0 ) GOTO 991
      IEMAX=-1
      IEMIN=2000000000
      NEV=0
C
 10   CONTINUE
        CALL UEGET(IDADUE,IR,IE,ISTMSK,IDZRN,IDZBO,CFNAME,CGNAME,CTAPE,
     >    CFCCOM,IERR)
        IF(IERR.EQ.1) GOTO 20
        IF(IERR.NE.0) GOTO 992
        IF(IR.EQ.(-1)) THEN     ! start-of-file information
          FILENAME=CFNAME
        ELSE
          NEV=NEV+1
          IF(LDDBG.GT.4 .AND.MOD(NEV,100).EQ.0 ) WRITE(*,1001) NEV
 1001     FORMAT('    Read event number',I8)
          IF( IE.GT.IEMAX ) IEMAX=IE
          IF( IE.LT.IEMIN ) IEMIN=IE
        ENDIF
      GOTO 10
C
 20   CONTINUE
C&IF IBMAIX
C&      CALL ERRMSG('NOT_IMPLEMENTED','D0DAD_INFO','Cannot append under'
C&     > //' IBM AIX','F')
C&ELSE
      OPEN(IDADOK,NAME=INFO_NAME(1:LENOCC(INFO_NAME)),
     >   STATUS='UNKNOWN',ACCESS='APPEND')
C&ENDIF
      WRITE(IDADOK,1002) IR,IEMIN,IEMAX,FILENAME(1:LENOCC(FILENAME))
 1002 FORMAT(I7,2I8,1X,'!',A)
      CLOSE(IDADOK)
C
  999 CONTINUE
      ierr=0
      RETURN
C
 991  CONTINUE
      IERR=-1
      RETURN
C
 992  CONTINUE
      IERR=-2
      RETURN
C
 993  CONTINUE
      IERR=-3
      RETURN
      END
