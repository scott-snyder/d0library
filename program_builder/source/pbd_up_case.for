      SUBROUTINE PBD_UP_CASE ( INPSTRING,OUTSTRING )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Converts the input string to upper case letters
C-
C-   Inputs  : INPSTRING - Input string 
C-   Outputs : OUTSTRING - Converted string STRING
C-   Controls: 
C-
C-   Created  21-JUN-1991   Hyon Joo Kehayias
C-   Updated   8-Jan-1996 sss - compile with g77.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------

      CHARACTER*(*) INPSTRING	! INPUT STRING
      CHARACTER*(*) OUTSTRING	! OUTPUT STRING
      INTEGER*2 LENGTH          ! LENGTH OF INPUT STRING
      INTEGER   CHARNUM		! ASCII CHARACTER IN INTEGER FORMAT
      INTEGER*2 I               ! INDEX VARIABLE

      LENGTH = LEN ( INPSTRING )

      DO I = 1, LENGTH

        IF ( INPSTRING(I:I) .GE. 'a' .AND. 
     &       INPSTRING(I:I) .LE. 'z' ) THEN
C
C         Convert ASCII character to integer number
C
          CHARNUM = ICHAR( INPSTRING (I:I) )
C
C         Convert low case letter ('61'x - '7A'x ) to upper case letter
C        ( '41'x - '5A'x )
C
          CHARNUM = CHARNUM - 32
          OUTSTRING(I:I) = CHAR( CHARNUM )

        ELSE

          OUTSTRING(I:I) = INPSTRING(I:I)
         
        END IF

      END DO

      RETURN
      END

