      SUBROUTINE PBD_CHK_PACK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  This routine checks the input package names,
C-                          reads combined packages if any, and generates 
C-                          package names table to be used by all other 
C-                          modules.
C-
C-   Inputs  : Input package names
C-   Outputs : Package names table
C-   Controls: Common error flag ERROR_FLAG
C-            ( .TRUE. if fatal error, .FALSE. if no error ) 
C-
C-   Modules called by this routine:  PBD_IS_VALID, PBD_MSG, PBD_READ_FILE 
C-                                    
C-   Created  28-MAY-1993   Hyon Joo Kehayias
C-        
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$PROGRAM_BUILDER$INC:PBD_COMMON.INC'

      INTEGER*2 OFFSET            ! OFFSET TO PACKAGE NAME TABLE
      INTEGER*2 FILE_TYPE         ! FILE TYPE TO READ
      INTEGER*2 I                 ! INDEX VARIABLE
 
      CHARACTER*80 ERROR_BUF      ! ERROR MESSAGE BUFFER
      CHARACTER*80 MESSAGE        ! MESSAGES
      CHARACTER*80 MSG_BUF        ! MESSAGE BUFFER
C
      DATA ERROR_BUF /
     & '-W-Duplicate package name '/
C
      DATA MESSAGE /
     & '-- Validating input package names --'/
C
      ERROR_FLAG = .FALSE.
      CALL PBD_MSG ( MESSAGE )
C
C     If no package name entered and combined package name exists,
C     set up combined package file name to read
C
      IF ( TOT_INP_PACK .EQ. 0 .AND. COMBINED_LEN .GT. 0 ) THEN
        COMB_PACk_NAME = COMBINED_NAME
        COMB_PACK_LEN = COMBINED_LEN
C
C       Read the combined package file and generate package name table
C
        FILE_TYPE = COMBINED
        CALL PBD_READ_FILE ( FILE_TYPE )
        RETURN
C
      END IF
C
C     If combined name not entered, generate the combined name from 
C     the package name for later use
C
      IF ( TOT_INP_PACK .EQ. 1 .AND. COMBINED_LEN .EQ. 0 ) THEN
C
C       Check if combined package name
C
        IF ( LOC_PACK_NAME(1)(1:1) .EQ. '%' ) THEN
          COMBINED_NAME = LOC_PACK_NAME(1)(2:)
          COMBINED_LEN = LOC_PACK_LEN(1)-1

        ELSE IF ( LOC_PACK_NAME(1)(LOC_PACK_LEN(1):LOC_PACK_LEN(1))
     &       .EQ. '%' ) THEN

          COMBINED_NAME = LOC_PACK_NAME(1)(1:LOC_PACK_LEN(1)-1)
          COMBINED_LEN = LOC_PACK_LEN(1)-1

        ELSE
          COMBINED_NAME = LOC_PACK_NAME(1)
          COMBINED_LEN = LOC_PACK_LEN(1)
        END IF

      END IF
C
C     Set up Global Package Names table from the input package names
C
      DO I = 1, TOT_INP_PACK
C
C       Check if duplicate package name entered
C
        IF ( NUMPACK .GT. 0 ) THEN
          CALL PBD_IS_VALID ( LOC_PACK_NAME(I), PACKAGE_NAME, 
     &                      NUMPACK, OFFSET )
        ELSE
          OFFSET = 0                    ! No need to check
        END IF

        IF ( OFFSET .EQ. 0 ) THEN       ! Not duplicate package name
C
C         Check if combined package name
C
          IF ( LOC_PACK_NAME(I)(1:1) .EQ. '%' ) THEN ! Combined package name

            COMB_PACk_NAME = LOC_PACK_NAME(I)(2:)
            COMB_PACK_LEN  = LOC_PACK_LEN(I)-1
C
C           Read the combined package file and generate package name table
C
            FILE_TYPE = COMBINED
            CALL PBD_READ_FILE ( FILE_TYPE )
            IF ( ERROR_FLAG ) RETURN

          ELSE IF ( LOC_PACK_NAME(I)(LOC_PACK_LEN(I):LOC_PACK_LEN(I))
     &       .EQ. '%' ) THEN

            COMB_PACk_NAME = LOC_PACK_NAME(I)(1:LOC_PACK_LEN(I)-1)
            COMB_PACK_LEN  = LOC_PACK_LEN(I)-1
C
C           Read the combined package file and generate package name table
C
            FILE_TYPE = COMBINED
            CALL PBD_READ_FILE ( FILE_TYPE )
            IF ( ERROR_FLAG ) RETURN

          ELSE                            ! Not combined package name
            NUMPACK = NUMPACK + 1
            PACKAGE_NAME(NUMPACK) = LOC_PACK_NAME(I)
            PACK_NAME_LEN(NUMPACK) = LOC_PACK_LEN(I)
          END IF

        ELSE
C
C         Duplicate package name, output a warning message
C
          MSG_BUF = ERROR_BUF(1:26) // LOC_PACK_NAME(I)
     &              (1:LOC_PACK_LEN(I))//' - Ignored'
          CALL PBD_MSG ( MSG_BUF )
        END IF

      END DO

      RETURN
      END
