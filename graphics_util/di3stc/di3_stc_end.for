      SUBROUTINE STC_END
C----------------------------------------------------------------------
C-   Purpose and Methods : End DI-3000 this session (but allow
C-     accumulation to continue).
C-   Created  11-NOV-1990   Michael Shupe
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:DI3STC.INC'
C----------------------------------------------------------------------
      INPROG=0                          ! FLAG STC_PLOT THAT REINIT
                                        ! NEEDED
      CALL JDEVOF(1)
      CALL JEND
  999 RETURN
      END
