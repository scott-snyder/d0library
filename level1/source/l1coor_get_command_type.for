      SUBROUTINE L1COOR_GET_COMMAND_TYPE(LINE, COMMAND, GOOD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get the first two words of the COOR message, and
C-      return them as an integer parameter.
C-
C-   Inputs  : LINE     The input line
C-   Outputs : COMMAND  The command found (integer parameter)
C-             GOOD     Whether the input was valid
C-   Controls: none
C-
C-   Created  30-JUL-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-   Updated  18-NOV-1991   Philippe Laurens, Steven Klocek   
C-                      Modified to take its input from a string rather than a
C-                      file.
C-   Updated   9-DEC-1991   Philippe Laurens, Steven Klocek  
C-                      Added keywords dealing with Level 1.5 programming 
C-   Updated  16-SEP-1992   Philippe Laurens, Steven Klocek  
C-                      Added keywords dealing with end-of-run file 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:L1COOR_PARSER.PARAMS'
      INCLUDE 'D0$INC:L1COOR_PARSER.INC'
      CHARACTER*1 L1COOR_NEXT_CHAR
      EXTERNAL L1COOR_NEXT_CHAR
C
      INTEGER COMMAND
      CHARACTER*(*) LINE
      LOGICAL GOOD
C
      CHARACTER*20 COMMANDS(1:MAX_COOR_COMMAND), COMMAND_STR
      INTEGER MAX_LENGTH 
      PARAMETER (MAX_LENGTH = MAX_COOR_COMMAND_LENGTH)
      INTEGER COUNT
      CHARACTER*1 CHAR
C
C
      GOOD = .TRUE.
C
C       Commands here are of fixed length
      COMMAND_STR = LINE(1:MAX_LENGTH)
C
C       There are many 'WRT_HOST' commands, and we want to ignore all of them.
C
      IF (COMMAND_STR(1:10) .EQ.  '  WRT_HOST') THEN
        COMMAND = COOR_WRTHOST
        GOTO 999
      ENDIF
C
C       Find a matching string
      DO COUNT = 1, MAX_COOR_COMMAND
        IF (COMMAND_STR .EQ. COMMANDS(COUNT)) THEN
          COMMAND = COUNT
          GOTO 999
        ENDIF
      END DO
C
C       Only will get here if the string was not matched

      GOOD = .FALSE.
C
C----------------------------------------------------------------------
  999 RETURN
C
      ENTRY L1COOR_INIT_COMMAND_TYPE()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize the strings of the possible
C-                           COOR messages.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  30-JUL-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-   Updated   3-DEC-1991   Philippe Laurens, Steven Klocek   
C-                      Added Level 1.5 COOR Commands.
C-   Updated  28-JUN-1993   Philippe Laurens - MSU L1 Trigger   
C-                      Add messages for Large Tiles 
C-
C----------------------------------------------------------------------
      COMMANDS(COOR_ACQBANDW) = '  AUTOTUNE ACQBANDW'
      COMMANDS(COOR_ANDORREQ) = '  SPECTRIG ANDORREQ'
      COMMANDS(COOR_AUTODIS)  = '  SPECTRIG  AUTODIS'
      COMMANDS(COOR_AUTOTUNE) = '  SPECTRIG AUTOTUNE'
      COMMANDS(COOR_DGTZOFF)  = '  GEO_SECT DGTZ_OFF'
      COMMANDS(COOR_EMETCNT)  = '  THRESHLD  EMETCNT'
      COMMANDS(COOR_EMETSUM)  = '  THRESHLD  EMETSUM'
      COMMANDS(COOR_EML2SUM)  = '  THRESHLD  EML2SUM'
      COMMANDS(COOR_ENABLE)   = '  SPECTRIG   ENABLE'
      COMMANDS(COOR_EXIT)     = '      EXIT'
      COMMANDS(COOR_FEBZDIS)  = '  SPECTRIG  FEBZDIS'
      COMMANDS(COOR_FREE)     = '  SPECTRIG     FREE'
      COMMANDS(COOR_HDETSUM)  = '  THRESHLD  HDETSUM'
      COMMANDS(COOR_HDL2SUM)  = '  THRESHLD  HDL2SUM'
      COMMANDS(COOR_INITIAL)  = '   INITIAL'
      COMMANDS(COOR_OBEYBUSY) = '  SPECTRIG OBEYBUSY'
      COMMANDS(COOR_OBEYLEV2) = '  SPECTRIG OBEYLEV2'
      COMMANDS(COOR_PAUSE)    = '     PAUSE'
      COMMANDS(COOR_PRESCALE) = '  SPECTRIG PRESCALE'
      COMMANDS(COOR_RD_TIME)  = '  SPECTRIG  RD_TIME'
      COMMANDS(COOR_REENABLE) = '  SPECTRIG REENABLE'
      COMMANDS(COOR_RESETSCL) = '  SPECTRIG RESETSCL'
      COMMANDS(COOR_RESUME)   = '    RESUME'
      COMMANDS(COOR_RSEMET)   = '    REFSET     EMET'
      COMMANDS(COOR_RSHDVETO) = '    REFSET   HDVETO'
      COMMANDS(COOR_RSTOTET)  = '    REFSET    TOTET'
      COMMANDS(COOR_STARTDGT) = '  SPECTRIG STARTDGT'
      COMMANDS(COOR_STOP)     = '      STOP'
      COMMANDS(COOR_TOTETCNT) = '  THRESHLD TOTETCNT'
      COMMANDS(COOR_TOTETSUM) = '  THRESHLD TOTETSUM'
      COMMANDS(COOR_TOTL2SUM) = '  THRESHLD TOTL2SUM'
      COMMANDS(COOR_VSEMLIST) = '  ST_VS_RS  EM_LIST'
      COMMANDS(COOR_VSTOTLIST)= '  ST_VS_RS TOT_LIST'
      COMMANDS(COOR_XCEMTOWER)= '   EXCLUDE  EMTOWER'
      COMMANDS(COOR_XCHDTOWER)= '   EXCLUDE  HDTOWER'
      COMMANDS(COOR_MISPTSUM) = '  THRESHLD MISPTSUM'
      COMMANDS(COOR_L15TYPE)  = '  SPECTRIG L15_TYPE'
      COMMANDS(COOR_L15TERM)        = '  SPECTRIG L15_TERM'
      COMMANDS(COOR_WRTHOST)  = '  WRT_HOST'
      COMMANDS(COOR_RSLGTILE) = '    REFSET LRG_TILE'
      COMMANDS(COOR_VSLGTILE) = '  ST_VS_RS LRG_TILE'
      COMMANDS(L15COOR_SYLOAD)= '  L15CTSYS LOADCODE'
      COMMANDS(L15COOR_SYSTRT)= '  L15CTSYS    START'
      COMMANDS(L15COOR_REFSET)= '  L15CTERM   REFSET'
      COMMANDS(L15COOR_LOCDSP)= '  L15CTERM  LOC_DSP'
      COMMANDS(L15COOR_GLBDSP)= '  L15CTERM GLOB_DSP'
      COMMANDS(L15COOR_FRMCOD)= '  L15CTERM FRAMECOD'
      COMMANDS(L15COOR_STVSTM)= '  L15CTERM ST_VS_TM'
C
      RETURN
C----------------------------------------------------------------------
      END
