      SUBROUTINE QUIT
C-----------------------------------------------------------------
C-                                                               -
C-     Terminate gracefully cleaning up screen                   -
C-                                                               -
C-  ENTRY QUITS: quit without asking                             -
C-                                                               -
C-          SDP Dec.,1986                                        -
C-----------------------------------------------------------------
C
      IMPLICIT NONE
      LOGICAL YES
C
      CALL OUTMSG(' ARE YOU SURE YOU WANT TO QUIT?')
      CALL GETPAR(1,' Y/N >','L',YES)
      IF(YES) THEN
        CALL UQUIT       ! user clean-up
        CALL EVCLWO('ALL')
        CALL EXIMEN(1,1)
        CALL EXIT        
      ENDIF
      RETURN
C
      ENTRY QUITS
      CALL UQUIT       ! user clean-up
      CALL EXIMEN(1,1)
      CALL EVCLWO('ALL')
      CALL EXIT
      RETURN
      END
