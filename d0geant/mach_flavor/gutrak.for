      SUBROUTINE GUTRAK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : User routine to control tracking of one track
C-                              ==>Called by : GTREVE
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   ??
C-   Updated   6-DEC-1988   A.M.Jonckheere  Added LV0 calls and made into
C-                              standard form.
C-   Updated   5-JUN-1989   Harrison B. Prosper
C-   Added hook LUTRAK
C-   Updated  31-AUG-1991   K. Wyatt Merritt   Added LUTRA1 call,which
C-                          had gotten lost in 3.14 version
C-   Updated  29-JUN-1992   K. Wyatt Merritt  Generalize LUTRA1 to 
C-                           SET_SHOWER_PARAM 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:GCBANK.INC'
      INCLUDE 'D0$INC:GCKINE.INC/LIST'
      INCLUDE 'D0$INC:GCLINK.INC'
      INCLUDE 'D0$INC:GCFLAG.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INCLUDE 'D0$INC:DCALOG.INC'
C
C----------------------------------------------------------------------
C
      IF (.NOT.LBAT.AND.IDEBUG.EQ.1.AND.DTRK.NE.2.AND.ISTAK.EQ.0) THEN
        CALL GPKINE(ITRA)
      ENDIF
C
      CALL SET_SHOWER_PARAM
C
      CALL GTRACK
C
C ****  Check if pause has been requested
C&IF VAXELN
C&      CALL WAIT_NOPAUSE
C&ENDIF
C call routines to handle detector operations after tracking a single
C track
C
C ***********************
C ****  USER HOOK LUTRAK
C ***********************
      CALL LUTRAK
C
  999 RETURN
      END
