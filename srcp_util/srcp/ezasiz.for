      SUBROUTINE EZASIZ (LUNIN,LUNOUT,FILNAM,NVALS,NPARS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create a new RCP file with the \SIZE parameter
C-                         inserted at the start of the file. The input
C-                         file should be already OPEN. The input file is
C-                         first rewound and EZSIZE is called to calculate
C-                         the size of the file. The file is rewound again
C-                         and its records are transferred to a new file
C-                         after the size parameter has been written out.
C-
C-                         NOTE: The sizes written are 10 greater than
C-                               calculated.
C-
C-   Inputs  : LUNIN    Unit number of input file; file MUST already be OPEN.
C-             LUNOUT   Unit number of output file; opened/closed internally.
C-             FILNAM   Name of output file (opened with STATUS='NEW')
C-
C-   Outputs : NVALS    Calculated number of values in output file
C-             NPARS    Calculated number of parameters (+ comments)
C-                      in output file.
C-
C-   Controls: None
C-
C-   Created  19-JAN-1989   Harrison B. Prosper, Chip Stewart
C-   Updated  25-MAR-1990   Harrison B. Prosper
C-      For RCP files with \START parameter add \SIZE after that parameter.
C-   Updated  28-MAR-1990   Harrison B. Prosper
C-      Corrected overwrite bug
C-   Updated  12-Feb-1992   Herbert Greenlee
C-      UNIX version.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LUNIN,LUNOUT,NVALS,NPARS
      CHARACTER*(*) FILNAM
C
      INTEGER II,JJ,LL,L
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:SRCP.DEF'
C----------------------------------------------------------------------
      CHARACTER*(CHRCRD) RECORD,STRING,INPUT_FILE
      LOGICAL OK
C----------------------------------------------------------------------
      L = LEN(FILNAM)
C
C ****  Scan file and determine size parameters
C
      REWIND LUNIN
      CALL EZSIZE (LUNIN,NVALS,NPARS)
      REWIND LUNIN
C
      NVALS = NVALS + 1
      NPARS = NPARS + 1
C
      CALL D0OPEN (LUNOUT,FILNAM(1:L),'OFL',OK)
      IF(.NOT.OK)CALL D0_ABORT(' D0OPEN failed')
C
C ****  Check for \START parameter, and write it out if present
C
      READ (UNIT=LUNIN,FMT='(A)',END=800) RECORD
      CALL UPCASE (RECORD,STRING)
      IF ( INDEX(STRING,'\START') .GT. 0 ) THEN
        CALL SWORDS (RECORD,II,JJ,LL)
        WRITE(UNIT=LUNOUT,FMT='(A)') RECORD(1:JJ)
      ELSE
        REWIND LUNIN                    ! Go back to start of file
      ENDIF
C
C ****  Write out new \SIZE parameter
C
      WRITE(UNIT=LUNOUT,FMT='(A6,2I10)') '\SIZE ',NVALS+10,NPARS+10
C
C ****  If next record contains a \SIZE parameter ignore it.
C
      READ (UNIT=LUNIN,FMT='(A)',END=800) RECORD
      CALL UPCASE (RECORD,STRING)
      IF ( INDEX(STRING,'\SIZE') .EQ. 0 ) THEN
        CALL SWORDS (RECORD,II,JJ,LL)
        WRITE(UNIT=LUNOUT,FMT='(A)') RECORD(1:JJ)
      ENDIF
C
C ****  Transfer remaining records from input to output file
C
  100 CONTINUE
      READ (UNIT=LUNIN,FMT='(A)',END=800) RECORD
      CALL SWORDS (RECORD,II,JJ,LL)
      WRITE(UNIT=LUNOUT,FMT='(A)') RECORD(1:JJ)
      GOTO 100
C
  800 CONTINUE
      CLOSE(UNIT=LUNOUT)
C
  999 RETURN
      END
