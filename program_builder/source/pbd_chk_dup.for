      SUBROUTINE PBD_CHK_DUP(TOKEN,CUR_PACK,PACKINDX,INTINDX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : This routine searches the interface names table to
C-                         check if the same interface name already
C-                         exists from other packages. 
C-
C-   Inputs  : TOKEN - Interface name to check
C-             CUR_PACK - Current package # being processed
C-   Outputs : PACKINDX - Package number where the same interface name found 
C-             INTINDX  - Hook number where the same interface name is used 
C-   Controls: 
C-
C-   Created  13-SEP-1991   Hyon Joo Kehayias
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$PROGRAM_BUILDER$INC:PBD_COMMON.INC'
      CHARACTER*(*) TOKEN               ! INPUT INTERFACE NAME
      INTEGER*2 CUR_PACK                ! CURRENT PACKAGE #
      INTEGER*2 PACKINDX                ! PACKAGE NUMBER WHERE INT NAME FOUND
      INTEGER*2 INTINDX                 ! HOOK INDEX OF INTERFACE NAME
      INTEGER*2 LENGTH                  ! STRING LENGTH
      INTEGER*2 I,J                     ! INDEX VARIABLES
      LOGICAL FOUND                     ! LOOP CONTROL FLAG
C
C     Get the length of the interface name
C      
      LENGTH = LEN(TOKEN)
C
C     Set default return values
C
      PACKINDX = 0
      INTINDX = 0

      FOUND = .FALSE.
C
C     Search through all interface names in every package
C
      DO I = 1, CUR_PACK
        J = 1
        DO WHILE ( J .LE. NUMHOOK .AND. .NOT. FOUND )
          IF ( TOKEN(1:LENGTH) .EQ. 
     &         INT_NAME(I,J)(1:INT_NAME_LEN(I,J)) ) THEN
            FOUND = .TRUE.
C
C           Save return values - package number where 
C           the interface name found and the hook index
C
            PACKINDX = I
            INTINDX = J
          ELSE
            J = J + 1
          END IF
        END DO
      END DO

      RETURN
      END
