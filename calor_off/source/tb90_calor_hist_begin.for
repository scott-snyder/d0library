      FUNCTION TB90_CALOR_HIST_BEGIN ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  To modify TB90_CALOR_HIST parameters at
C-                           the begining of a run
C-
C-   Returned value  : TRUE if OK
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  13-AUG-1990   Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL TB90_CALOR_HIST_BEGIN,EZERR
      LOGICAL TB90_CALOR_HIST_MENU
      LOGICAL TB90_CALOR_HIST_BEGIN_RUN
      REAL    ETA_BEAM,PHI_BEAM,LIMITS(2,4)
      INTEGER IER,NPAR
      CHARACTER MSG*80,LABELS(2)*20,TYPAR(2)*1
      LOGICAL PBD_TEMP_FLAG,VALUE
      CHARACTER*64 MENNAM,MENCOM
      CHARACTER OFF*2
      LOGICAL FLGVAL,OK,FIRST
      INTEGER I,J,K,L
      CHARACTER*40 COMMAND
      DATA FIRST/.TRUE./
      DATA OFF/'X-'/
C----------------------------------------------------------------------
      TB90_CALOR_HIST_BEGIN = .TRUE.
      CALL EXAMINE_DISPATCH_COMMAND(COMMAND)
      IF (COMMAND .NE. 'TB90_CALOR_HIST') GOTO 999
C----------------------------------------------------------------------
      CALL EZPICK('TB90_CALOR_HIST_RCP')
      IF(EZERR(IER)) GOTO 999
      NPAR = 2
      CALL EZGET('ETA_OF_BEAM',ETA_BEAM,IER)
      IF(IER.NE.0) THEN
         CALL ERRMSG('PARAMETER NOT IN RCP','TB90_CALOR_HIST_BEGIN',
     &    ' NO ETA PARAMETER','W')
        GOTO 999
      END IF
      CALL EZGET('PHI_OF_BEAM',PHI_BEAM,IER)
      IF(IER.NE.0) THEN
        CALL ERRMSG('PARAMETER NOT IN RCP','TB90_CALOR_HIST_BEGIN',
     &    ' NO PHI PARAMETER','W')
        GOTO 999
      END IF
  
C      WRITE(MSG,100)ETA_BEAM
C  100 FORMAT(5X,'ETA OF BEAM ',F10.3)
C      CALL INTMSG(MSG)
C      CALL GETPAR(1,' ETA OF BEAM > ','R',ETA
C      IF (ETA.NE.0) CALL EZSET('ETA_OF_BEAM',ETA,IER)
        
      LABELS(1) = ' ETA OF BEAM >'
      LABELS(2) = ' PHI OF BEAM >'
      DO I = 1, NPAR
       TYPAR(I) = 'R'
      END DO
      
      LIMITS(1,1) = 12.0  !LOW ETA
      LIMITS(2,1) = 39.0  !HIGH ETA
      LIMITS(1,2) = 1.0   !LOW PHI
      LIMITS(2,2) = 64.0  !HIGH PHI

      CALL GETDIS(2,LABELS,TYPAR,LIMITS,ETA_BEAM,PHI_BEAM)

      CALL EZSET('ETA_OF_BEAM',ETA_BEAM,IER)
      CALL EZSET('PHI_OF_BEAM',PHI_BEAM,IER)
      TB90_CALOR_HIST_BEGIN = TB90_CALOR_HIST_BEGIN_RUN ()
      CALL EZRSET
  999 RETURN
      ENTRY TB90_CALOR_HIST_MENU ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TO WRITE THE MENU FOR TB90_CALOR_HIST 
C- 
C-
C-   Inputs  : NONE
C-   Outputs : NONE
C-   Controls: NONE
C-
C-   Created  14-AUG-1990   Chip Stewart
C-
C----------------------------------------------------------------------
      TB90_CALOR_HIST_MENU = .TRUE.

      IF ( PBD_TEMP_FLAG('TB90_CALOR_HIST',VALUE) ) THEN
        MENNAM = 'TB90_CALOR_HIST Params'
        MENCOM = 'TB90_CALOR_HIST'
        IF ( .NOT. VALUE ) THEN
          MENNAM = OFF//'TB90_CALOR_HIST Params'
          MENCOM = OFF//'TB90_CALOR_HIST'
        ENDIF
        CALL MENADD('EXAMINE',.TRUE.,
     &   MENNAM,
     &   MENCOM,
     &   '        Modify the cuts used in filling histograms'//
     &   ' in the TB90_CALOR_HIST pacakage. ETA & PHI of the'//
     &   ' beam in units of PHYSICS ADDRESS are originally s'//
     &   'et in the RCP file and are able to be re-set here.'//
     &   ' '   )
      END IF
 1999 RETURN
      END
