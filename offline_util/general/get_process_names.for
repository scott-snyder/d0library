      SUBROUTINE GET_PROCESS_NAMES (MAXNAM,NAME,NUMBER,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return a list of the names of all the 
C-   processes on the current node. The names are obtained by 
C-   invoking the command SHOW SYSTEM in a sub-process. 
C-   NOTE:The unit number 80 is used internally.
C-
C-   Inputs  : MAXNAM   [I]     Maximum size of character array NAME
C-            
C-   Outputs : NAME(*)  [C*]    Array of process names
C-             NUMBER   [I]     Number of processes
C-             IER      [I]     Status code 
C-   Controls: 
C-
C-   Created  22-MAR-1990   Harrison B. Prosper
C-   Updated  21-Feb-1992   Herbert Greenlee
C-      UNIX version
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER MAXNAM
      CHARACTER*(*) NAME(*)
      INTEGER NUMBER
      INTEGER IER
C&IF VAXVMS
      INTEGER LUN
      PARAMETER( LUN = 80 )
      CHARACTER*80 RECORD
      INTEGER I,J,K,II,JJ,KK
      LOGICAL MORE_NAMES
C&ELSE
C&ENDIF
C----------------------------------------------------------------------
      IER = 0
      NUMBER = 0
C
C&IF VAXVMS
      CALL LIB$SPAWN('SHOW SYSTEM/OUTPUT=PROCESS.LIS')
      OPEN (UNIT=LUN, FILE='PROCESS.LIS',STATUS='OLD',ERR=900)
      READ (UNIT=LUN,FMT='(A)') RECORD
      READ (UNIT=LUN,FMT='(A)') RECORD
C
C ****  Loop over process names
C
      NUMBER = 0
      MORE_NAMES = .TRUE.
      DO WHILE ( MORE_NAMES ) 
        READ (UNIT=LUN,FMT='(A)',END=800) RECORD
        CALL WORD (RECORD,I,J,K)
        RECORD = RECORD(J+1:)
        CALL WORD (RECORD,I,J,K)
        NUMBER = NUMBER + 1
        NAME(NUMBER) = RECORD(I:J)
        MORE_NAMES = NUMBER .LT. MAXNAM
      ENDDO
      IER = -2                          ! Reached buffer limit
  800 CONTINUE
C
      CLOSE(UNIT=LUN,DISP='DELETE')
      GOTO 999
C
  900 CONTINUE
      IER = -1                          ! Cannot find file
C&ELSE
C&ENDIF
  999 RETURN
      END
