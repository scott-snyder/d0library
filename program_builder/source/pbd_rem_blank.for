      SUBROUTINE PBD_REM_BLANK ( STRING, LENGTH )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : This routine removes all blanks from input string
C                          and returns the trimmed input string and new length
C-
C-   Inputs  : Input string - STRING
C-   Outputs : Trimmed input string - STRING
C-             Length of trimmed output string - LENGTH
C-   Controls: 
C-
C-   Created  01-JUL-1991   Hyon Joo Kehayias
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      CHARACTER*(*) STRING              ! INPUT STRING
      INTEGER*2 LENGTH                  ! INPUT STRING LENGTH
      CHARACTER*1 BLANK                 ! BLANK CHARACTER
      INTEGER*2 I,J                     ! INDEX VARIABLES
      DATA BLANK /' '/
      
      LENGTH = LEN ( STRING )
      J = 0

      DO I = 1, LENGTH

        IF ( STRING (I:I) .NE. BLANK) THEN
          J = J + 1
          STRING(J:J) = STRING(I:I)
        END IF

      END DO
C
C     Save the new length
C
      LENGTH = J

      RETURN
      END

