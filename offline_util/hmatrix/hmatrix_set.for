      SUBROUTINE HMATRIX_SET(NAME,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set all hmatrix links etc. to point to
C-   the named HMATRIX. Use HMATRIX_RESET to reset to the previously
C-   set HMATRIX. Note: HMATRIX_SET ... HMATRIX_RESET can be nested.
C-
C-   Inputs  : NAME     [C*]    Name identifying HMATRIX
C-
C-   Outputs : IER      [I]     Error code; 0 if OK.
C-
C-      ENTRY HMATRIX_SHOW(NAME)
C-      ENTRY HMATRIX_RESET
C-
C-   Created  22-JAN-1991   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) NAME
      CHARACTER*132 CTEMP
      INTEGER IER
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZHMATRIX.INC'     ! Link area
      INCLUDE 'D0$INC:NHMATRIX.INC'     ! Hmatrix names
      INCLUDE 'D0$INC:IHMATRIX.INC'     ! Hmatrix stack
C----------------------------------------------------------------------
      INTEGER JBANK,ID
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        HMTR_PTR = 0
      ENDIF
C
      IER = 0
C
C ****  Find address of specified Hmatrix (HMTR)
C
      CALL HMATRIX_FIND(NAME,JBANK,ID)
      IF ( JBANK .LE. 0 ) THEN
        IER =-2
        CTEMP = 'No HMTR bank with name '//NAME
        CALL ERRMSG
     &    ('HMATRIX','HMATRIX_FIND',CTEMP,'W')
        GOTO 999
      ELSE
C
C ****  Add index of given hmatrix to stack
C
        IF ( HMTR_STACK_PTR .LT. HMTR_STACK_MAX ) THEN
          HMTR_STACK_PTR = HMTR_STACK_PTR + 1
        ENDIF
        HMTR_STACK(HMTR_STACK_PTR) = ID
C
C ****  Update link area and bank sizes
C
        HMTR_PTR = ID
        LHMTR = JBANK                 ! Update link area
        CALL HMATRIX_SET_LINKS
        CALL HMATRIX_SET_SIZES
      ENDIF
      RETURN
C
      ENTRY HMATRIX_RESET
C----------------------------------------------------------------------
C-   Purpose and Methods : Reset HMTR path.
C----------------------------------------------------------------------
      IF ( HMTR_STACK_PTR .GT. 1 ) THEN
        HMTR_STACK_PTR = HMTR_STACK_PTR - 1   ! Decrement stack pointer
      ENDIF
C
C ****  Update link area
C
      HMTR_PTR = HMTR_STACK(HMTR_STACK_PTR)
      LHMTR = HMTR_LINKS(HMTR_PTR)
      CALL HMATRIX_SET_LINKS
      CALL HMATRIX_SET_SIZES
      RETURN
C
      ENTRY HMATRIX_SHOW (NAME)
C----------------------------------------------------------------------
C-   Purpose and Methods : Return the name of current HMATRIX
C----------------------------------------------------------------------
      IF ( HMTR_PTR .GT. 0 ) THEN
        NAME = HMTR_NAME(HMTR_PTR)
      ELSE
        NAME = ' '
        CALL ERRMSG('HMATRIX','HMATRIX_SHOW','No Hmatrix selected','W')
      ENDIF
  999 RETURN
      END
