      SUBROUTINE DIABOX(TITLE,NPROMPT,PROMPT,OUTSTR,LENSTR,FLAG)
C----------------------------------------------------------------
C-
C-  Purpose and Method: Display a dynamically created dialog box.
C-     This routine uses MODIFY_PARAMS
C-
C-  Inputs:  TITLE     [C*]: Title of DIALOG box
C-           NPROMPT    [I]: Number of prompts strings
C-           PROMPT [C*(*)]: Prompt strings
C-           OUTSTR [C*(*)]: Initial values
C-
C-  Outputs: OUTSTR [C*(*)]: Output strings
C-           LENSTR [ I(*)]: Length of strings
C-
C-  Controls:FLAG      [C*]: Is not utilized
C-  
C----------------------------------------------------------------
      CHARACTER*(*) TITLE,FLAG
      CHARACTER*(*) PROMPT(*),OUTSTR(*)
      INTEGER NPROMPT, LENSTR(*)
C
      CHARACTER*80 REM(20)
      LOGICAL   MODFLG(20)
C----------------------------------------------------------------
C
C *** Displaying the strings and letting user 
C *** modify them
C
      CALL MODIFY_PARAMS(NPROMPT,PROMPT,OUTSTR,REM,MODFLG)
      
  999 RETURN
      END
