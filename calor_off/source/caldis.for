      LOGICAL FUNCTION CALDIS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Execute the D0 event DISPLAY for the
C-   Calorimeter ONLY. Call CALDIS_OFF to turn OFF the package, 
C-   and CALDIS_ON to turn it on again.
C-
C-   Returned value  : TRUE
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   7-FEB-1990   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL NOMORE,PXEXEC
      LOGICAL CALDIS_ON,CALDIS_OFF,PCEXEC
C----------------------------------------------------------------------
      CALDIS = .TRUE.
      CALL PXMAIN(NOMORE)
      CALL PBD_SET_FLAG ('CALDIS',.NOT.NOMORE)
      RETURN
C
      ENTRY CALDIS_ON
      CALL PBD_SET_FLAG ('CALDIS',.TRUE.)
      RETURN
C
      ENTRY CALDIS_OFF
      CALL PBD_SET_FLAG ('CALDIS',.FALSE.)
      RETURN
C
      ENTRY PXEXEC
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : PIXIE hook: Calls PCEXEC only.
C-
C-   Returned value  : TRUE if PCEXEC returns TRUE.
C----------------------------------------------------------------------
      PXEXEC = PCEXEC()
  999 RETURN
      END
