      LOGICAL FUNCTION D0DAD_SKIP_RUN(IRUN)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Check to see if run IRUN is on the D0DAD
C-     run skip list.
C-
C-   Inputs  : IRUN - Run number
C-   Outputs :
C-   Controls:
C-
C-   Created   7-Jun-1995   John D. Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:d0dadcom.inc'
      LOGICAL D0DAD_SETUP_SKIP_RUNS
C- Parameters
      INTEGER IRUN
      CHARACTER*(*) CRUN_LIST
C- Local control arrays
      INTEGER NSKIP_MAX
      PARAMETER(NSKIP_MAX=25)
      INTEGER SKIP_LIST(NSKIP_MAX),NSKIP
C- Local temporary storage
      INTEGER I,FIRST,LAST,IC
      CHARACTER*10 CRUN
C- External functions
      INTEGER LENOCC
C- Save and initial condition
      DATA NSKIP/0/
      SAVE SKIP_LIST,NSKIP
C-----------------------------------------------------------------------
C
      D0DAD_SKIP_RUN = .FALSE.
      DO I=1,NSKIP
        IF( SKIP_LIST(I).EQ.IRUN) D0DAD_SKIP_RUN = .TRUE.
      ENDDO
      GOTO 999
C
C-----------------------------------------------------------------------
      ENTRY D0DAD_SETUP_SKIP_RUNS(CRUN_LIST)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Define the skip list.  No syntax checking,
C-     be careful.
C-
C-----------------------------------------------------------------------
      IF( LENOCC(CRUN_LIST).LE.0 ) GOTO 999
C
C  Convert the skip list to local form and setup pointers
C
      FIRST=1
      LAST=LENOCC(CRUN_LIST)
      IF( CRUN_LIST(FIRST:FIRST).EQ.'(' ) FIRST=2
      IF( CRUN_LIST(LAST:LAST).EQ.')' ) LAST=LAST-1
      DO I=FIRST,LAST
        IF( CRUN_LIST(I:I).EQ.',' ) CRUN_LIST(I:I)=' '
      ENDDO
C
      CRUN=' ' 
      CALL GETWORD(CRUN_LIST(FIRST:LAST),NSKIP+1,CRUN)
      DO WHILE( LENOCC(CRUN).GT.0 ) 
        IF( NSKIP.EQ.NSKIP_MAX ) THEN
          WRITE(D0DAD_ERRTXT,9001) 
 9001     FORMAT('Number of runs to skip exceeds internal limit.')
          IF( LDDBG.GT.0 .AND. NSKIP.EQ.NSKIP_MAX ) 
     >       CALL ERRMSG('InternalOverflow''D0DAD_SETUP_SKIP_RUNS',
     >       D0DAD_ERRTXT,'W')
        ELSE
          NSKIP=NSKIP+1
          READ(CRUN,*) SKIP_LIST(NSKIP)
          CRUN=' '
          CALL GETWORD(CRUN_LIST(FIRST:LAST),NSKIP+1,CRUN)
        ENDIF
      ENDDO
C
      IF( LDDBG.GT.10 ) THEN
        WRITE(*,1000) NSKIP
 1000   FORMAT('  D0DAD_SETUP_SKIP_RUNS: Skipping ',I3,' runs.',/,
     >         '    Runs')
        DO I=1,NSKIP
          WRITE(*,1001) SKIP_LIST(I)
 1001     FORMAT('  ',I6)
        ENDDO
      ENDIF
C
  999 RETURN
      END
