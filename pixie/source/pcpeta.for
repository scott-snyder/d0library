      SUBROUTINE PCPETA
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Pick a IETA segment on the CAL SIDE View
C-
C-   Controls: by window coordinates of picked point

C-
C-
C-   Created  14-SEP-1994   Nobuaki Oshima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:PXCOMK.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INTEGER IETA, IER
      REAL    XP(3), THE, ETA
      LOGICAL EZERROR
C-
C----------------------------------------------------------------------
C-
C
C ****  Get window coordinates of picked point
C
      CALL PU_GET_PICKW(XP)
C
C ****  Getting Theta
C
      THE = ATAN2(XP(2),XP(1))
      THE = ABS(THE) 
      ETA = -LOG(TAN(THE/2.))
      IF (ETA.GT.3.7) THEN
        ETA=3.7
      ELSEIF(ETA.LT.-3.7)THEN
        ETA=-3.7
      ENDIF
      IETA = ETA*10.
      IF (ETA .GE. 0.) THEN
        IETA = IETA + 1
      ELSE
        IETA = IETA - 1
      ENDIF
C-
C--- Update CAL ETA and CAL DETA in PX_CALDIS_RCP
C-
      CALL EZPICK('PX_CALDIS_RCP')          ! Selecting CALDIS bank
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PCPETA','Bank PX_CALDIS_RCP NOT FOUND','W')
        GOTO 999
      ENDIF
C-
C--- Impose IETA region in the CAL END view
C-
      CALL PUSETV('CAL IETACEN',IETA)
C-
      CALL EZRSET
C-
C--- Update Theta CENTER and WIDTH in PX_SYSTEM_RCP for GLOBAL Mode
C--- Good for Calorimeter only, so far...   
C-
C--- Impose IETA region in the D0 END view
C-
  999 RETURN
      END

