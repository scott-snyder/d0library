      SUBROUTINE PBD_GET_COM( TOKEN,LENGTH, COM_NAME, COM_SIZE,
     &  COM_LEN, RETFLAG ) 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : This routine validates zebra common size
C-                         provided by user input.  If any error, 
C-                         an error message is displayed.
C-
C-   Inputs  : TOKEN    - Input zebra bank size 
C-             LENGTH   - Length of token
C-             COM_NAME - zebra common name
C-             
C-   Outputs : COM_SIZE - zebra common size string
C-             COM_LEN  - Length of COM_SIZE string
C-
C-   Controls: RETFLAG  - Error return flag
C-             ( .TRUE. if any error, .FALSE. if no error )
C-
C-   Modules called by this routine:  PBD_MSG
C-  
C-   Based on the PASCAL procedure Build_Initialization_Routine of the old
C-   Program Builder.
C-

C-   Created  02-JUL-1991   Hyon Joo Kehayias
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------

      INCLUDE 'D0$PROGRAM_BUILDER$INC:PBD_COMMON.INC'

      CHARACTER*(*) TOKEN             ! INPUT TOKEN
      CHARACTER*(*) COM_NAME          ! ZEBRA COM NAME STRING
      CHARACTER*(*) COM_SIZE          ! ZEBRA COM SIZE STRING
      INTEGER*4 NUMBER                ! CONVERTED INTEGER NUMBER
      INTEGER*2 LENGTH                ! TOKEN STRING POS AND LENGTH
      INTEGER*2 COM_LEN               ! ZEBRA COM SIZE STRING LENGTH
      CHARACTER*28 ERROR_BUF(3)       ! ERROR MESSAGE 
      CHARACTER*80 ERROR_MSG          
      LOGICAL RETFLAG                 ! ERROR RETURN FLAG 
      DATA ERROR_BUF /
     &     ' size out of range',
     &     '-E-Invalid input string for ',
     &     '-E-Empty string for '/
      
      RETFLAG = .FALSE.               ! Set default to no error
      IF ( LENGTH .GT. 0 ) THEN
C
C       Convert input token to integer number
C
        READ ( TOKEN(1:LENGTH), 100, ERR=999 ) NUMBER
100     FORMAT ( I8 )
C
C       Check if the zebra bank size is within its range
C
        IF ( NUMBER .LE. 0 .OR. NUMBER .GT. MAXCOM ) THEN
          ERROR_MSG = '-E-'//COM_NAME//ERROR_BUF(1)(1:18)
          CALL PBD_MSG (ERROR_MSG)
          RETFLAG =.TRUE.
        ELSE
          COM_SIZE = TOKEN
          COM_LEN = LENGTH
        END IF

      ELSE
C
C       Null string for the zebra bank size, send error message
C
        COM_SIZE = '  '
        COM_LEN = 0
        ERROR_MSG = ERROR_BUF(3)(1:20)//COM_NAME//' size'
        CALL PBD_MSG (ERROR_MSG)
        RETFLAG =.TRUE.
C
      END IF
      RETURN

999   CONTINUE
C
C     Conversion error
C
      ERROR_MSG = ERROR_BUF(2)(1:28)//COM_NAME//' size'
      CALL PBD_MSG (ERROR_MSG)
      RETFLAG =.TRUE.

      RETURN
      END

