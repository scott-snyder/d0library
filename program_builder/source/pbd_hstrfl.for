      SUBROUTINE PBD_HSTRFL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Generates FORTRAN code for the PBD production 
C-                         history subroutine HSTRFL by calling PBD_HST_BANK.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: Common error flag ERROR_FLAG
C-            ( .TRUE. if fatal error, .FALSE. if no error ) 
C-
C-   Modules called by this routine:  PBD_HST_BANK,PBD_MSG
C-                                    DATE, TIME
C-   Created  14-MAR-1992   Hyon Joo Kehayias
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------

      INCLUDE 'D0$PROGRAM_BUILDER$INC:PBD_COMMON.INC'
C
C     Local Data Area
C
      INTEGER*2 I                       ! INDEX VARIABLE
      CHARACTER*80 FILE_NAME            ! FILE NAME
      CHARACTER*80 MESSAGE(2)           ! DISPLAY MESSAGES
      CHARACTER*80 MSG_BUF              ! MESSAGE BUFFER
      CHARACTER*80 ERROR_BUF            ! ERROR MESSAGE BUFFER
      CHARACTER*9  DATE_STR             ! ASCII DATE 
      CHARACTER*8  TIME_STR             ! ASCII TIME
      CHARACTER*2  NUMSTR               ! ASCII NUMBER
      LOGICAL OPENED                    ! OPEN STATUS

      DATA MESSAGE /       
     & '-- Generating FORTRAN Source File ',
     & '-- Building HSTRFL Routine for '/
      DATA ERROR_BUF /
     & '-E-File open error for '/

C
C     Determine the FORTRAN source file name
C
      FOR_FILE_NAME = COMBINED_NAME(1:COMBINED_LEN)//'_'//
     &                 FRAME_NAME(1:FRAME_LEN)
      FOR_FILE_LEN = COMBINED_LEN+1+FRAME_LEN

      FILE_NAME = FOR_FILE_NAME(1:FOR_FILE_LEN)//'.FOR'

      MSG_BUF = MESSAGE(1)(1:34)//FILE_NAME
      CALL PBD_MSG ( MSG_BUF )
C
C     Open the FORTRAN source file
C
      CALL D0OPEN(8, FILE_NAME, 'OFL', OPENED)

 10   IF ( .NOT. OPENED ) THEN    ! Open error
        MSG_BUF = ERROR_BUF(1:23)//FILE_NAME
        ERROR_FLAG = .TRUE.
        CALL PBD_MSG ( MSG_BUF )
        RETURN
      END IF

      MSG_BUF = MESSAGE(2)(1:31)//FRAME_NAME(1:FRAME_LEN)
      CALL PBD_MSG ( MSG_BUF )
C
C     Get current date and time
C
      CALL DATE ( DATE_STR )
      CALL TIME ( TIME_STR )
      DATE_TIME = COMMENT_STR//DATE_STR//' '//TIME_STR

100   FORMAT ( A )
      WRITE (8,100) COMMENT_LINE 
      WRITE (8,100) COMMENT_LINE
      OUTLINE = COMMENT_STR//'Created by the PROGRAM BUILDER' 
      WRITE (8,100) OUTLINE
      WRITE (8,100) DATE_TIME
      WRITE (8,100) COMMENT_LINE
      WRITE (8,100) COMMENT_LINE 
      WRITE (8,100) COMMENT_LINE 
C
C     Generate HISTORY_BANK routine
C
      CALL PBD_HST_BANK

      CLOSE ( 8 )
      RETURN
      END
