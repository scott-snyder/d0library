      SUBROUTINE PBD_IS_VALID (TOKEN,TABLE,SIZE,OFFSET)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : This routine searches each entry of the character 
C-                         string array TABLE for the input string TOKEN.  
C-                         If the input string found in the table, it returns 
C-                         the offset to the table.  Otherwise it returns 0.
C-
C-   Inputs  : TOKEN - INPUT STRING FOR SEARCH
C-             TABLE - CHARACTER TABLE NAME TO SEARCH AGAINST 
C-             SIZE  - TOTAL # OF ENTRIES IN TABLE
C-
C-   Outputs : OFFSET - OFFSET TO TABLE WHERE THE INPUT TOKEN FOUND
C-             ( = >0  if token found 
C-               =  0  if token not found ) 
C-   Controls: 
C-
C-   Created  02-JUL-1991   Hyon Joo Kehayias
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INTEGER*2 SIZE                    ! TABLE SIZE
      INTEGER*2 OFFSET                  ! OFFSET TO TABLE WHERE TOKEN FOUND
      CHARACTER*(*) TOKEN               ! TOKEN TO BE RETURNED
      CHARACTER*(*) TABLE(SIZE)         ! TABLE TO SEARCH
      INTEGER*2 I                       ! INDEX VARIABLE
      LOGICAL FOUND                     ! LOOP CONTROL FLAG

      OFFSET = 0        ! MAKE DEFAULT OFFSET TO 0      
      FOUND = .FALSE.
      I = 1
      DO WHILE ( I .LE. SIZE .AND. .NOT. FOUND )
        IF ( TOKEN .EQ. TABLE(I) ) THEN
          FOUND = .TRUE.
          OFFSET = I
        ELSE
          I = I + 1
        END IF
      END DO

      RETURN
      END
