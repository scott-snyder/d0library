      SUBROUTINE MUON_L2_PARAMETERS
     &         (NEWPAR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TOOL_PARAM Routine for MUON_L2 
C-
C-   Inputs  : NEWPAR : Number of parameter sets to read
C-   Outputs : None
C-   Controls: None
C-
C-   Created   9-NOV-90   by the L2STATE program
C-             9-NOV-90   And Modified By H. T. Diehl 
C-            17-DEC-1990 NOW USES LSL2H TREE (HTD)
C-            27-JUN-1991 AND NOW MOVES SMUO BANK FROM SL2H TO STPC
C-            04-AUG-1991 AND NOW ITS SET UP FOR CRC OR PBARP
C-            23-NOV-1991 Version for VMS_FILTERS and ONLINE (HTD)
C-            03-MAR-1992 Add parameter TRACK_IN_ROAD (HTD)
C-            28-JUN-1992 Add parameters MUON_QUALITY (HTD)
C-            01-JUL-1992 Change TRACK_IN_ROAD from Log to Char var (HTD)
C-            08-NOV-1992 Add call to L2_VERT_PARAMETERS (HTD)
C-            08-DEC-1992 ADD CALL TO SAMUS_L2_PARAMETERS (HTD)
C-	      04-NOV-1993 Add cosmic_reject, refit_t0, scint_on_track,
C-		          cal_on_track, cd_on_track parameters (PZQ)
C----------------------------------------------------------------------
      IMPLICIT NONE

      INCLUDE 'D0$INC:MUON_L2_PARAMS.INC'
      INCLUDE 'D0$INC:MUON_L2_PARAMS_C.INC'
      INCLUDE 'D0$LINKS:IZ2SMUO.LINK'
      INCLUDE 'D0$LINKS:IZMUON_L2_RCP.LINK'

      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSL2H.LINK' 
      INCLUDE 'D0$LINKS:IZSMUO.LINK' 
      INCLUDE 'D0$LINKS:IZSTPC.LINK' 

      BYTE NEWPAR
      CHARACTER*52 ERROR_MESSAGE
      CHARACTER*60 OUT_MESSAGE
      INTEGER IER,STR_LEN,I
      INTEGER LSL2H,GZSL2H
      INTEGER LSTPC
      INTEGER LOLD_SMUO,LNEW_SMUO

C----------------------------------------------------------------------

      IF(NEWPAR.LE.0) GOTO 999

C----------------------------------------------------------------------
C- DO SAMUS_L2_PARAMETERS
C----------------------------------------------------------------------

      CALL SAMUS_L2_PARAMETERS

C----------------------------------------------------------------------
C- DO MUON_L2_PARAMETERS
C----------------------------------------------------------------------


      LSL2H = GZSL2H()    

      IF(LSL2H.LE.0) THEN
        ERROR_MESSAGE= 'COULD NOT FIND LSL2H. STOP.'
        GOTO 1000
      ELSEIF(LC(LSL2H-IZMUON_L2_RCP) .LE. 0) THEN
        ERROR_MESSAGE= 'LSL2H-IZMUON_L2_RCP IS LE 0. STOP.'
        GOTO 1000
      ENDIF

      LSTPC = LC(LSTPH-IZSTPC)
      LOLD_SMUO = LC(LSL2H-IZ2SMUO)
      LNEW_SMUO = LC(LSTPC-IZSMUO)
      CALL ZSHUNT(IXSTP,LOLD_SMUO,LSTPC,-IZSMUO,1)

      CALL EZPICK('MUON_UTIL_L2_PARAMS')
      CALL EZERR(IER)
      IF(IER.NE.0) THEN
        ERROR_MESSAGE=' YOU STRUCK OUT AT EZPICK MUON_UTIL_L2_PARAMS.'
        GOTO 1000
      ENDIF
      CALL EZRSET

      CALL EZPICK('MUON_L2_TRGR') ! WE DONT HAVE ANYTHING HERE YET
      CALL EZERR(IER)
      IF(IER.NE.0) THEN
        ERROR_MESSAGE= 'COULD NOT FIND MUON_L2_TRGR_RCP'
        GOTO 1000
      ENDIF
      CALL EZRSET

      CALL EZPICK('MUON_L2')
      CALL EZERR(IER)
      IF (IER.NE.0) GO TO 999	   ! could not find muon_parameters_rcp

      CALL EZGET('NUMBER_OF_SETS',NUMBER_OF_SETS,IER)
      IF(NUMBER_OF_SETS.GT.128) THEN
        ERROR_MESSAGE= 'NUMBER OF SETS TOO BIG'
        GOTO 1000
      ENDIF                 
      CALL EZGET('NUM_MUONS',NUM_MUONS,IER)
      IF(IER.NE.0) THEN
        ERROR_MESSAGE= 'COULD NOT FIND NUM_MUONS'
        GOTO 1000
      ENDIF
      CALL EZGET('PTMIN',PTMIN,IER)
      IF(IER.NE.0) THEN
        ERROR_MESSAGE= 'COULD NOT FIND PTMIN'
        GOTO 1000
      ENDIF
      CALL EZGET('ABS_ETA_MAX',ABS_ETA_MAX,IER)
      IF(IER.NE.0) THEN
        ERROR_MESSAGE= 'COULD NOT FIND ABS_ETA_MAX'
        GOTO 1000
      ENDIF
      DO I = 1, NUMBER_OF_SETS
        CALL EZGETS('TRACK_IN_ROAD',I,TRACK_IN_ROAD(I),STR_LEN,IER)
        IF(IER.NE.0) THEN
          TRACK_IN_ROAD(I) = 'FALSE'
          ERROR_MESSAGE= 'COULD NOT FIND TRACK_IN_ROAD'
          WRITE(OUT_MESSAGE,1001)ERROR_MESSAGE,IER
          CALL ERRMSG('MUON_L2_PARAMETERS','TRACK_IN_ROAD',
     $      OUT_MESSAGE,'W')
        ENDIF
      ENDDO
      DO I = 1, NUMBER_OF_SETS
        CALL EZGETS('MUON_QUALITY',I,MUON_QUALITY(I),STR_LEN,IER)
        IF(IER.NE.0) THEN
          MUON_QUALITY(I) = 'LOOSE'
          ERROR_MESSAGE= 'COULD NOT FIND MUON_QUALITY'
          WRITE(OUT_MESSAGE,1001)ERROR_MESSAGE,IER
          CALL ERRMSG('MUON_L2_PARAMETERS','MUON_QUALITY',
     $      OUT_MESSAGE,'W')
        ENDIF
      ENDDO
      CALL EZGET('COSMIC_REJECT',COSMIC_REJECT,IER)
      IF(IER.NE.0) THEN
        DO I = 1, NUMBER_OF_SETS
          COSMIC_REJECT(I) = .FALSE.
        ENDDO
	ERROR_MESSAGE= 'COULD NOT FIND COSMIC_REJECT'
	WRITE(OUT_MESSAGE,1001)ERROR_MESSAGE,IER
	CALL ERRMSG('MUON_L2_PARAMETERS','COSMIC_REJECT',
     $      OUT_MESSAGE,'W')
      ENDIF
      CALL EZGET('REFIT_T0',REFIT_T0,IER)
      IF(IER.NE.0) THEN
        DO I = 1, NUMBER_OF_SETS
          REFIT_T0(I) = .FALSE.
        ENDDO
	ERROR_MESSAGE= 'COULD NOT FIND REFIT_T0'
	WRITE(OUT_MESSAGE,1001)ERROR_MESSAGE,IER
	CALL ERRMSG('MUON_L2_PARAMETERS','REFIT_T0',
     $      OUT_MESSAGE,'W')
      ENDIF
      CALL EZGET('SCINT_ON_TRACK',SCINT_ON_TRACK,IER)
      IF(IER.NE.0) THEN
        DO I = 1, NUMBER_OF_SETS
          SCINT_ON_TRACK(I) = .FALSE.
        ENDDO
	ERROR_MESSAGE= 'COULD NOT FIND SCINT_ON_TRACK'
	WRITE(OUT_MESSAGE,1001)ERROR_MESSAGE,IER
	CALL ERRMSG('MUON_L2_PARAMETERS','SCINT_ON_TRACK',
     $      OUT_MESSAGE,'W')
      ENDIF
      CALL EZGET('CAL_ON_TRACK',CAL_ON_TRACK,IER)
      IF(IER.NE.0) THEN
        DO I = 1, NUMBER_OF_SETS
          CAL_ON_TRACK(I) = .FALSE.
        ENDDO
	ERROR_MESSAGE= 'COULD NOT FIND CAL_ON_TRACK'
	WRITE(OUT_MESSAGE,1001)ERROR_MESSAGE,IER
	CALL ERRMSG('MUON_L2_PARAMETERS','CAL_ON_TRACK',
     $      OUT_MESSAGE,'W')
      ENDIF
      CALL EZGET('CD_ON_TRACK',CD_ON_TRACK,IER)
      IF(IER.NE.0) THEN
        DO I = 1, NUMBER_OF_SETS
          CD_ON_TRACK(I) = .FALSE.
        ENDDO
	ERROR_MESSAGE= 'COULD NOT FIND CD_ON_TRACK'
	WRITE(OUT_MESSAGE,1001)ERROR_MESSAGE,IER
	CALL ERRMSG('MUON_L2_PARAMETERS','CD_ON_TRACK',
     $      OUT_MESSAGE,'W')
      ENDIF
      CALL EZRSET
C
C...forcibly turn off hyperactive muon data format error messages
C      CALL ERRMAX('MUSRT2: ADC error',0,0)
C      CALL ERRMAX('MUSRT2: bad pad latch',0,0)
C
C- CALL OTHER INITIALIZATIONS
C
      CALL L2_VERT_PARAMETERS
      CALL CL2_INI
C----------------------------------------------------------------------
C- IF WE GOT TO HERE, WELL BE OK
C----------------------------------------------------------------------
  999 RETURN
 1001 FORMAT(A48,' IER ',I4)
C----------------------------------------------------------------------
 1000 CONTINUE
      WRITE(OUT_MESSAGE,1001)ERROR_MESSAGE,IER
      CALL ERRMSG('MUON_L2','MUON_L2_PARAMETERS',OUT_MESSAGE,'F')
      RETURN

      END
