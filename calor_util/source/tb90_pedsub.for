      SUBROUTINE TB90_PEDSUB(ICRATE,IWORD,PULSE_HEIGHT,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds the PEDestal for this channel and SUBtracts  
C-                         it from the pulse height part of the WORD. 
C-
C-   Inputs  : ICRATE       - ADC crate #
C-             IWORD        - with PED included 
C-   Outputs : PULSE_HEIGHT - PED subtracted
C-             IER   - Error code
C-                   0 - OK
C-                   -1 - no PED bank 
C-                   -2 - can't find channel in PED bank
C-                   
C-   Controls: none
C-
C-   Created  23-MAR-1990   Chip Stewart, Dharmaratna,W.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LCPD8,LCPD1,GZCPD8,GZCPD1
      INTEGER ICRATE,IWORD,CRATE,CARD,BLS,TOWER,DEPTH,SCALE
      INTEGER NEGLIM,IER,IDATA
      REAL    PEDESTAL,SIGMA,PULSE_HEIGHT
      LOGICAL FIRST,PED_FILE
      DATA FIRST/.TRUE./,PED_FILE/.TRUE./
C----------------------------------------------------------------------
      
      IF (.NOT. PED_FILE ) GOTO 998
      IF ( FIRST ) THEN
C
C ****  Need to find address everytime until we get a ped link area going.
C       
C        FIRST = .FALSE.
C
C ****   check for existing pedestal bank
C
        LCPD8 = GZCPD8 ()
        LCPD1 = GZCPD1 ()
        IF ( LCPD8.LE.0 .OR. LCPD1.LE.0 ) THEN
C
C ****  READ IN PEDESTALS
C
          CALL INTMSG(' NEED PEDESTAL FILE to SUBTRACT PEDS')
          CALL INTMSG(' Give DBL3 as the pedestal file name to read
     &    from DBL3')
          CALL SELECT_INPUT_FILES
          LCPD8 = GZCPD8 ()
          LCPD1 = GZCPD1 ()
          IF ( LCPD8.LE.0 .OR. LCPD1.LE.0) THEN
            PED_FILE = .FALSE.
            PEDESTAL = 0
            CALL INTMSG(' No PEDESTAL FILE selected - NO SUBTRACTION ')
            GOTO 999
          END IF
        END IF
    
      ENDIF
      CALL CADUPK(ICRATE,IWORD,CRATE,CARD,BLS,TOWER,DEPTH,SCALE,
     &  NEGLIM)
C
C--- Now get the SEQUENTIAL ADC address
C
      CALL GTCPD_ADDR(1,CRATE,CARD,BLS,TOWER,DEPTH,SCALE,
     &  PEDESTAL,SIGMA,IER)
  998 CALL CALPH(IWORD,IDATA)
      PULSE_HEIGHT = IDATA 
      PULSE_HEIGHT = PULSE_HEIGHT - PEDESTAL
  999 RETURN
      END
