      INTEGER FUNCTION CURONF(ONOFF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Turn ON/OFF cursor if possible. VAX-specific
C-
C-   Inputs  : ONOFF: Integer flag for on or off (1 for off, 0 for on)
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 26-SEP-1988   Jan S. Hoftun
C-   Updated     1-OCT-1991   Herbert Greenlee
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ONOFF
      INCLUDE 'D0$INC:SMGCOM.INC'
      LOGICAL ISTAT,SMG$SET_CURSOR_MODE,SMG$SET_OUT_OF_BAND_ASTS
      EXTERNAL CTRAST
C----------------------------------------------------------------------
      ISTAT=SMG$SET_CURSOR_MODE(PASTID,ONOFF)
C      IF(.NOT.ISTAT) THEN
C        CALL MSGSCR(ISTAT,'CURONF-->')
C      ENDIF
      CURONF=ISTAT
C&IF VAXVMS
      IF(ONOFF.EQ.0) THEN
        CURFLG=.FALSE.
        ISTAT=SMG$SET_OUT_OF_BAND_ASTS(PASTID,0,CTRAST)
      ELSE
        CURFLG=.TRUE.
        ISTAT=SMG$SET_OUT_OF_BAND_ASTS(PASTID,2**3+2**25,CTRAST)
      ENDIF
C&ENDIF
      RETURN
      END
