      SUBROUTINE EZPICK (BKNAME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Select specified SRCP bank so that
C-                         subsequent calls to the EZxxxx routines will
C-                         refer to this bank. EZPICK calls can be
C-                         nested to a maximum depth of 10.
C-
C-   Inputs  : BKNAME      Bank name. Up to 32 characters.
C-
C-   Outputs : NONE        Use EZERR to return error code
C-                         0 --- OK
C-                         See also EZGET_ERROR_TEXT
C-   Controls: None
C-
C-============================================================================
C-
C-   ENTRY EZRSET
C-
C-   Purpose and Methods : Re-select previous RCP bank
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-============================================================================
C-
C-   ENTRY EZPICK_GET_PTR
C-
C-   Purpose and Methods : Re-select previous RCP bank
C-
C-   Inputs  : None
C-   Outputs : IPTR   NUMBER OF ENTRIES IN STACK
C-             OVFLOW NUMBER OF ENTRIES IN OVERFLOW STACK
C-   Controls: None
C-
C-============================================================================
C-
C-   ENTRY EZPICK_ERRORS
C-
C-   Purpose and Methods : Change defaults on ezpick error messages
C-
C-   Inputs  : LEVEL  < 0   restore what was there previously
C-                    =0    turn off error messages
C-                    =1    default: error message if ezpick fails
C-                    =2    error messages if ezpick fails, OR if run out of
C-                          regular stack and start overflow stack
C-                    =3    messages for 1,2, OR if underflow stack (too many
C-                            EZRSETS)
C-   Outputs : None
C-   Controls: None
C-
C-
C-============================================================================
C-
C-   ENTRY EZPICK_DUMP_STACK
C-
C-   Purpose and Methods : dump list of currently active banks, if any
C-
C-   Inputs  : LUN    unit number to write to
C-   Outputs : None
C-   Controls: None
C-
C-   Created  23-SEP-1988   Harrison B. Prosper
C-   Updated   1-MAY-1990   Harrison B. Prosper
C-      Increase stack size
C-   Updated  21-MAR-1991   Harrison B. Prosper
C-      Add entry point EZPICK_GET_PTR and call to ERRMSG
C-      Add overflow stack
C-   Updated   2-APR-1991   Harrison B. Prosper
C-      Modify error handling.
C-   Updated   8-DEC-1994   James T. Linnemann  
C-        add EZPICK_DUMP_STACK entry
C-        make stack empty = 0 entries consistently
C-        add ERRMSGs for stack overflow and underflow
C-   Updated   3-Jan-1996   sss - Compile with g77.
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) BKNAME
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:LKSRCP.INC'
      INCLUDE 'D0$INC:NMSRCP.INC'
C----------------------------------------------------------------------
      CHARACTER*(CHRCRD) REMARK
C
      INTEGER ID,I,LUN
      INTEGER IPTR,OVFLOW
      INTEGER OVERFLOWS,OVFLOW_PTR
      INTEGER LAST_ERROR_LEVEL, ERROR_LEVEL, LEVEL
C
C ****  Overflow stack
C
      INTEGER MAXOSTACK
      PARAMETER( MAXOSTACK = 50 )
      INTEGER OSTACK(MAXOSTACK)
C
      LOGICAL ON
      SAVE LAST_ERROR_LEVEL,ERROR_LEVEL,OSTACK,OVFLOW_PTR
      DATA LAST_ERROR_LEVEL     /1/
      DATA ERROR_LEVEL          /1/
      DATA OVFLOW_PTR           /0/
C----------------------------------------------------------------------
      ENTRY SLSRCP (BKNAME)
C----------------------------------------------------------------------
C
C       ISRCP is the pointer to address of the
C       selected bank
C
C ****  Clear error flag
C
      ERRSRC = EZS_SUCCESS
C
      CALL EZZLOC (BKNAME(1:LEN(BKNAME)),LSRCP,ID)
      IF ( LSRCP .GT. 0 ) THEN
        IF ( ISTACK .LT. MXSTACK ) THEN
          ISTACK = ISTACK + 1           ! Increment stack pointer
          ISRCP  = ID
          KSTACK(ISTACK) = ISRCP        ! Put new bank ID onto stack
          OVFLOW_PTR = 0                ! Reset overflow pointer
          ERRSRC = EZS_SUCCESS
        ELSE
C
C ****  For stack overflows use overflow stack; things should
C ****  never get to this stage, but....!
C
          IF ( OVFLOW_PTR .LT. MAXOSTACK ) THEN
            OVFLOW_PTR = OVFLOW_PTR + 1
          ELSE
            ERRSRC = EZS_BANK_NOTSELECTED      ! bank found but stack full
            IF ( ERROR_LEVEL .GE. 1 ) THEN
              REMARK ='Unable to pick RCP bank '//BKNAME(1:LEN(BKNAME))
     &            //': missing EZRSETs?'
              CALL ERRMSG('RCP_STACK_LIMIT','EZPICK',REMARK,'W')
            ENDIF
          ENDIF
          ISRCP  = ID
          OSTACK(OVFLOW_PTR) = ISRCP     ! Put new bank ID onto OVFL stack
C
          IF ( ERROR_LEVEL .GE. 2 ) THEN
            REMARK =
     &        'EZPICK stack limit reached; using overflow stack;bank'
     &        //BKNAME(1:LEN(BKNAME))
            CALL ERRMSG('RCP_STACK_LIMIT','EZPICK',REMARK,'W')
          ENDIF
        ENDIF
      ELSE
        ERRSRC = EZS_BANK_NOTFOUND      ! Bank not found
        IF ( ERROR_LEVEL .GE. 1 ) THEN
          REMARK = 'Unable to pick RCP bank '//BKNAME(1:LEN(BKNAME))
          CALL ERRMSG('NO_RCP_BANK','EZPICK',REMARK,'W')
        ENDIF
      ENDIF
      RETURN
C
C ****  Entry point to re-select previous bank
C
      ENTRY EZRSET
      ENTRY RSSRCP
C
      ERRSRC = EZS_SUCCESS
C
C ****  If there are stack overflows then use overflow stack
C
      IF ( OVFLOW_PTR .GE. 1 ) THEN
        OVFLOW_PTR = OVFLOW_PTR - 1
        IF ( OVFLOW_PTR .GE. 1 ) THEN
          ISRCP = OSTACK(OVFLOW_PTR)    ! Use overflow stack
        ELSE
          ISRCP = KSTACK(ISTACK)        ! Use regular stack
        ENDIF
      ELSE                              ! pop regular stack if possible
        IF ( ISTACK .GE. 2 ) THEN       ! stack won't be empty after pop
          ISTACK = ISTACK - 1
          ISRCP = KSTACK(ISTACK)        ! Take current ID from top of stack
        ELSE                            ! stack will be empty when popped
          IF ((ISTACK.LE.0).AND.(ERROR_LEVEL.GE.3)) THEN
            REMARK = 'EZPICK stack underflowed: too many EZRSET calls'
            CALL ERRMSG('RCP_STACK_EXTRA_EZRSET','EZPICK',REMARK,'W')
          ENDIF
          ISTACK = 0                    ! back to stack empty condition
          ISRCP = 0                     ! no bank selected
        ENDIF
      ENDIF
      RETURN
C
      ENTRY EZPICK_GET_PTR(IPTR,OVFLOW)
      IPTR   = ISTACK
      OVFLOW = OVFLOW_PTR
      RETURN
C
      ENTRY EZPICK_ERRORS(LEVEL)
      IF ( LEVEL .LT. 0 ) THEN
        ERROR_LEVEL = LAST_ERROR_LEVEL  ! Restore error level
      ELSE
        LAST_ERROR_LEVEL = ERROR_LEVEL  ! Save error level
        ERROR_LEVEL = LEVEL
      ENDIF
      RETURN

      ENTRY EZPICK_DUMP_STACK(LUN)
      IF(ISTACK.GT.0) THEN
        CALL ERRMSG('RCP_STACK_NOT_EMPTY','EZPICK_DUMP_STACK',
     &      'Dumping EZPICK stack on request of caller','W')
        CALL EZDBUG(LUN)  !list all known banks
        DO I = 1,ISTACK
          ID = KSTACK(I)
          WRITE(LUN,100)I,MSRCP(ID)
  100     FORMAT(' Bank ',I4,' in the stack is ',A32)
        ENDDO
        IF(OVFLOW_PTR.GT.0) THEN
          WRITE(LUN,200) OVFLOW_PTR
  200     FORMAT(' There are ',I4,' more banks in the overflow stack')
          DO I = 1,OVFLOW_PTR
            ID = OSTACK(I)
            WRITE(LUN,100)I,MSRCP(ID)
          ENDDO
        ENDIF
      ENDIF
  999 RETURN
      END
