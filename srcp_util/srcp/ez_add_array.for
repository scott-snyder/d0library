      SUBROUTINE EZ_ADD_ARRAY(ARRAY_NAME,RECORDS,TOTAL_LINES,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Adds an array to the current RCP file
C-
C-   Inputs  : ARRAY_NAME  [C*]: Array  name
C-             RECORDS  [C*(*)]: Records of the array
C-             TOTAL_LINES  [I]: Total number of lines in the array
C-
C-   Outputs : IER          [I]: 0 If no errors
C-   Controls:
C-
C-   Created  18-JUN-1991   Lupe Howell
C-   Updated  11-DEC-1991   Lupe Howell  Error code fixed 
C-   Updated  12-Feb-1992   Herbert Greenlee
C-     Fixed for UNIX (eliminate concatenation argument).
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) ARRAY_NAME
      CHARACTER*(*) RECORDS(*)
      INTEGER TOTAL_LINES,IER
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:SRCP.DEF'
C----------------------------------------------------------------------
      CHARACTER*(CHRCRD) STRING
      INTEGER TL,I,TRULEN,IERR
C----------------------------------------------------------------------
      IER = 0
C
C ****  Adding the \ARRAY line
C
      STRING = '\ARRAY '//ARRAY_NAME
      CALL EZADD(STRING,1,IERR)
C
C ****  Adding the lines of the array
C
      DO I = 1, TOTAL_LINES
        TL = TRULEN(RECORDS(I))
        TL = MAX(1,TL)
        CALL EZADD(RECORDS(I)(1:TL),1,IERR)
        WRITE(4,*)RECORDS(I)(1:TL)
      ENDDO
C
C ****  Adding the \END line
C
      CALL EZADD('\END',1,IERR)
      IER = IERR
C
C ****  Checking for errors
C
      CALL EZEND
      IF(IERR.NE.0.AND.IERR.NE.EZS_BANK_EXTENDED)THEN
        CALL ERRMSG(' EZ_PACKAGE','EZ_ADD_ARRAY',
     &    ' Error adding to bank','W')
      ELSE
        IER = EZS_SUCCESS  ! No error
      ENDIF
  999 RETURN
      END
