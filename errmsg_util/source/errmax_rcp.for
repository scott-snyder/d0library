      SUBROUTINE ERRMAX_RCP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Reads a file, if existent, called errmsg_rcp and
C-   sequentially calls errmax with what it finds. it is intended to set a
C-   maximum number of error messages of a certain type.
C-
C-   Inputs  : Errmsg_rcp, list of messages to be suppressed. of the form:
C-
C-            \ARRAY Messages
C-            'specific message' count
C-            ....
C-            \END
C-   Outputs : Call ERRMAX('specific message', count, count)
C-   Controls: none
C-
C-   Created  26-APR-1993   R. J. Genik II
C-   Updated  25-JAN-1995   James T. Linnemann  more protection, ELN variant 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IDXX,ierr, name_len,max_count,eye, jay, int_val,var_type
      CHARACTER*80 msg_id,val_string
      LOGICAL ezerror,ok
C----------------------------------------------------------------------
C&IF VAXELN
C...In ELN, the RCP is downloaded, not read, and will change as needed
C&
C&    CALL EZPICK_NOMSG('ERRMSG_RCP',IER)
C&    OK = IER.EQ.0 !if bank picked successfully
C&ELSE
      CALL INRCP('ERRMSG_RCP', IERR)    !get whatever file is there now
      IF (IERR.NE.0) GO TO 999  ! No error messages, just go home
      CALL EZPICK('ERRMSG_RCP')
      OK = .NOT.EZERROR(IERR) !if bank picked successfully
C&ENDIF
      IDXX = 1
      DO WHILE (IERR.EQ.0)
        CALL CFill(' ',msg_id,1,80) ! set string to blanks
        CALL EZGET_Next_value_Type( 'MESSAGES', int_val, msg_id,
     +    var_type, name_len, IERR, IDXX)  !get identifier of message to supress
        IF((IERR.NE.0).OR.(var_type.LE.10)) GOTO 100 !need character variable
        CALL EZGET_Next_value_Type( 'MESSAGES', max_count, val_string,
     +    var_type, name_len, IERR, IDXX)  !get count after which to supress
        IF((IERR.NE.0).OR.(var_type.NE.1)) GOTO 100   ! should be an integer
        CALL Swords( msg_id, eye, jay, name_len)! remove leading and trailing
                                               ! blanks.
        CALL Errmax( msg_id(eye:jay),max_count,max_count) !supress message
  100 ENDDO
C&IF VAXELN
C...in ELN, do nothing: clearing of previous RCP banks done externally
C&
C&ELSE
      IF (OK) CALL ezdrop('ERRMSG_RCP')!forget it and be ready to get new
                                       ! RCP file next time around
C&ENDIF
      IF (OK) CALL EZRSET
  999 RETURN
      END
