      FUNCTION PRESET_MEMORY()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : PRESET ZEBCOM and/or ZEBSTP
C-
C-   Returned value  :
C-   Inputs  : PRESET_MEMORY_RCP
C-   Outputs : modified /zebcom/ and /zebstp/
C-   Controls:
C-
C-  ENTRY PRESET_CHECK()
C-    produces an error message if the contents has been altered from the preset
C-    value
C-
C-   Created  25-MAY-1993   James T. Linnemann
C-   Updated  24-JUN-1994   James T. Linnemann  use MZWORK to "protect" presets 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL PRESET_MEMORY,PRESET_CHECK
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER NASTY_NUMBER,IEVT_DIV,NWDS,IER,I,LOW,HI
      INTEGER NWDS_CHECK,NREF_STP
      PARAMETER (NWDS_CHECK = 100)    !number of words to check in IC
      PARAMETER( NREF_STP = 9 )     !number of ref links equived to ZCONS
      LOGICAL FIRST,EZERROR,OK,DO_ZEBCOM,DO_ZEBSTP
      CHARACTER*80 MSG
C
      SAVE FIRST,NASTY_NUMBER,IEVT_DIV,NWDS,DO_ZEBCOM,DO_ZEBSTP
      DATA FIRST / .TRUE. /
C----------------------------------------------------------------------
      IF( FIRST ) THEN
C
C...only get parameters once
        FIRST = .FALSE.
        CALL INRCP('PRESET_MEMORY_RCP',IER)   !NOTICE this calls INZSTP
        CALL EZPICK('PRESET_MEMORY_RCP')
        OK = .NOT.EZERROR(IER)
        DO_ZEBCOM = .FALSE.
        DO_ZEBSTP = .FALSE.
        NASTY_NUMBER= 0
        IEVT_DIV = 2
        NWDS = NWDS_CHECK !default
        IF (IER .EQ. 0) CALL EZGET('DO_ZEBSTP',DO_ZEBSTP,IER)
        IF (IER .EQ. 0) CALL EZGET('DO_ZEBCOM',DO_ZEBCOM,IER)
        IF (IER .EQ. 0) CALL EZGET('PRESET_TO',NASTY_NUMBER,IER)
        IF (IER .EQ. 0) CALL EZGET('EVENT_DIVISION',IEVT_DIV,IER)
        IF (IER .EQ. 0) CALL EZGET('NWDS',NWDS,IER)
        IF (OK) CALL EZRSET
C
C...this code writes into space which may be used for constants:
C...  only allow it once, i.e. before the constants get there
C...  If you choose this option, this package MUST run first.
C...   but notice that INZSTP has already been called above by INRCP
        IF (DO_ZEBSTP) THEN
          CALL UFILL(LC,1,200,NASTY_NUMBER)
          CALL CONSTP                          !kill RCP bank and start over
C
C... reserve space after ref links so FZIN/FZOUT won't use
          CALL MZWORK(IXSTP,ZCONS(NREF_STP+1),ZCONS(NWDS_CHECK),0) 
          CALL UFILL(ZCONS,1,NWDS_CHECK,NASTY_NUMBER) !but ref links aren't used
        ENDIF
      ENDIF
C
C... in principle allows resetting memory after readin, but doesn't seem
C    necessary since between events only the part used is wiped, not low memory
      IF (DO_ZEBCOM) THEN
C...  preset unused links
        CALL UFILL(LREF,1,NREF,NASTY_NUMBER)
        IF (IEVT_DIV.NE.2) IXMAIN = IXCOM + 1   !ONLY allow 1 as alternative
        IF (FIRST.OR.(IEVT_DIV.EQ.2)) THEN      !don't overwrite an event
C
C...reserve space after ref links so FZIN/OUT won't use this space
          CALL MZWORK(IXCOM,ZSTOR(1),ZSTOR(NWDS_CHECK),0)      
C...  preset any spare space in division 1
          CALL UFILL(ZSTOR,1,MAX(NWDS,NWDS_CHECK),NASTY_NUMBER)  
        ENDIF
      ENDIF
      PRESET_MEMORY = .TRUE.
      RETURN
C#######################################################################
      ENTRY PRESET_CHECK()
      IF (DO_ZEBSTP) THEN
        DO I = 1,NWDS_CHECK   !not safe to check links
          IF (ZCONS(I).NE.NASTY_NUMBER) THEN  ! ZCONS declared integer
            WRITE(MSG,100)I+99    !do translation from ZCONS to IC offsets
  100       FORMAT('DIVISION 1 ALTERED STARTING AT IC(',I3,')')
            CALL ERRMSG('ZEBSTP_OVERWRITE','PRESET_CHECK',MSG,IER)
            GO TO 150
          ENDIF
        ENDDO
      ENDIF
  150 CONTINUE
      IF (DO_ZEBCOM) THEN
        HI = NWDS_CHECK
        IF (IEVT_DIV.EQ.1) HI = 0
        LOW = -(NREF-1) !offset of LREF(1) from ZSTOR(1)
        DO I = LOW,HI    !should be safe to check ref links: not used
          IF (ZSTOR(I).NE.NASTY_NUMBER) THEN  ! ZSTOR declared integer
            WRITE(MSG,200)I+3   ! IQ(1) vs ZSTOR(1) offset per PRESET_MEMORY.DOC
  200       FORMAT('DIVISION 1 ALTERED AT STARTING IQ(',I3,')')
            CALL ERRMSG('ZEBCOM_OVERWRITE','PRESET_CHECK',MSG,IER)
            GO TO 250
          ENDIF
        ENDDO
      ENDIF
  250 CONTINUE
      PRESET_CHECK = .TRUE.
  999 RETURN
      END
