      SUBROUTINE UEGET(ILUN,IRUN,IEVT,IMSK,IZRN,IZBO,CFNAM,CGNAM,
     +  CTAP,CCOM,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get a data record from an unsorted d0dad
C-      event catalog.  If the record is not a new file entry, do not
C-      modify the name parameters (See D0DAD_UPDATE).
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   8-NOV-1993   John D Hobbs
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:zebcom.inc'
      INCLUDE 'D0$INC:d0dadcom.inc/NOLIST'
      INTEGER ILUN,IRUN,IEVT,IMSK(*),IZRN,IZBO,IER
      CHARACTER*(*) CFNAM,CGNAM,CTAP,CCOM
      CHARACTER*132 UELINE
      INTEGER  LENOCC
      EXTERNAL LENOCC
C----------------------------------------------------------------------
C
      CALL UELSET(ILUN,IER)
      IF( IER.NE.0 ) THEN
        IER = -4
        GOTO 999
      ENDIF
C
C  Read next line, skipping headers and blanks
C
 10   CONTINUE
      READ(ILUN,'(A)',ERR=997,END=998) UELINE
      IF( UELINE(1:8).EQ.'D0DAD UE' ) GOTO 10
      IF( LENOCC(UELINE).EQ.0 ) GOTO 10
      IF( UELINE(1:1).EQ.'*' ) GOTO 10
C
      IF( UELINE(1:1).EQ.'!') THEN
        READ(UELINE,9001,ERR=996) CFNAM
        READ(ILUN,'(A)',ERR=997,END=998) UELINE
        READ(UELINE,9001,ERR=996) CGNAM
        READ(ILUN,'(A)',ERR=997,END=998) UELINE
        READ(UELINE,9001,ERR=996) CTAP
        READ(ILUN,'(A)',ERR=997,END=998) UELINE
        READ(UELINE,9001,ERR=996) CCOM
 9001   FORMAT('!',A)
        IRUN = -1
      ELSE
        READ(UELINE,*,ERR=995) IRUN,IEVT,IMSK(1),IMSK(2),IZRN,IZBO
      ENDIF
C
      IF( LDDBG.GT.10 .AND. IRUN.EQ.-1) 
     +                      WRITE(*,1001) CFNAM,CGNAM,CTAP,CCOM
 1001 FORMAT(' UEGET: Filename: ',A50,'...',/,
     +       '        Generic: ',A60,'...',/,
     +       '        Tape: "',A20,'"',/,
     +       '        Comment: "',A40,'"',/)
C
 999  CONTINUE
      IER=0
      RETURN

 995  CONTINUE
      IF( LDDBG.GT.0 ) WRITE(*,*) ' UEGET: Error reading UE'
      IER=-1
      RETURN

 996  CONTINUE
      IF( LDDBG.GT.0 ) WRITE(*,*) ' UEGET: Error reading UE'
      IER=-2
      RETURN

 997  CONTINUE
      IF( LDDBG.GT.0 ) WRITE(*,*) ' UEGET: Error reading UE'
      IER=-3
      RETURN

 998  CONTINUE
      IF( LDDBG.GT.10 ) write(*,*) ' UEGET: End-of-file reading UE'
      IER=1
      RETURN

      END
