      SUBROUTINE PXBUILD_INIT(VERSION,MENU,ROUTINE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write INIT interface routine.
C-
C-   Inputs  : VERSION  [C*]    PXBUILD version number
C-             MENU     [C*]    Menu/Package Name
C-             ROUTINE  [C*]    Routine Name
C-            
C-   Outputs : None
C-   Controls: None
C-
C-   Created  12-SEP-1990   Harrison B. Prosper
C-   Updated   9-NOV-1990   LUPE HOWELL  , Harrison B. Prosper 
C-   Updated   7-MAY-1991   Lupe Howell  The %STPFILE template was added 
C-   Updated  24-JAN-1992   Lupe Howell  Update for SGI 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) VERSION
      CHARACTER*(*) MENU
      CHARACTER*(*) ROUTINE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:PXBUILDCOM.INC'
C----------------------------------------------------------------------
      INTEGER I,J,K,L,N,NLINES,IER,ROW,COLUMN,II,JJ,III,JJJ
      INTEGER PFNUM,LR,LM,LV,ISTART,IEND,X,Y,Z

C
      LOGICAL EZERROR
      CHARACTER*23 DAY
      CHARACTER*80 TEMPLATE(MAXLINE),STRING,TMP_STR
C----------------------------------------------------------------------
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      SAVE FIRST
C----------------------------------------------------------------------
C
C ****  Read template
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('PXBUILD_RCP')
        IF ( EZERROR(IER) ) THEN
          GOTO 999
        ELSE
          CALL EZ_GET_CHARS('INIT_TEMPLATE',NLINES,TEMPLATE,IER)
          CALL EZRSET
        ENDIF
      ENDIF
C
      CALL WORD(VERSION,I,J,LV)
      CALL WORD(ROUTINE,I,J,LR)
      CALL WORD(MENU,I,J,LM)
C
C ****  Get date
C
      CALL LIB$DATE_TIME(DAY)
C
C ****  Replace placeholders with tokens
C
      DO I =  1,NLINES
        LINE(I) = TEMPLATE(I)
      ENDDO
      CALL SWAP_TOKEN('%R',ROUTINE(1:LR),NLINES,LINE,LLINE)
      CALL SWAP_TOKEN('%P',MENU(1:LM),NLINES,LINE,LLINE)
      TMP_STR = 'PX_'//MENU(1:LM)//'.RCP'
      CALL WORD(TMP_STR,X,Y,Z)
      CALL SWAP_TOKEN('%F1',TMP_STR(1:Z),NLINES,LINE,LLINE)
      TMP_STR = '''PX_'//MENU(1:LM)//'_RCP'''
      CALL WORD(TMP_STR,X,Y,Z)
      CALL SWAP_TOKEN('%F2',TMP_STR(1:Z),NLINES,LINE,LLINE)
      CALL SWAP_TOKEN('%D',DAY(1:11),NLINES,LINE,LLINE)
      CALL SWAP_TOKEN('%V',VERSION(1:LV),NLINES,LINE,LLINE)

      STRING = '''PIXIE'',''INRCP'',''Unable to open PX_'//
     &  MENU(1:LM)//'_RCP'',''F'''
      CALL SWORDS(STRING,I,L,J)
      CALL SWAP_TOKEN('%ERRMSG_INRCP',STRING(1:L),NLINES,LINE,LLINE)

      STRING = '''PIXIE'',''EZ_SETUP_COMPACK'',''No PX_'//
     &  MENU(1:LM)//'_RCP bank'',''F'''
      CALL SWORDS(STRING,I,L,J)
      CALL SWAP_TOKEN('%ERRMSG_COMPACK',STRING(1:L),NLINES,LINE,LLINE)
      STRING = MENU(1:2)//'ISTP'
      CALL SWORDS(STRING,I,L,J)
      CALL SWAP_TOKEN('%STPROT',STRING(1:L),NLINES,LINE,LLINE)
      STRING = ''''//MENU(1:3)//'_STPFILE'''
      CALL SWORDS(STRING,I,L,J)
      CALL SWAP_TOKEN('%STPFILE',STRING(1:L),NLINES,LINE,LLINE)
C
C ****  Open output file
C
      TMP_STR = ROUTINE(1:LR)//'.FOR'
      CALL PXOPEN(LUN,TMP_STR,'O',IER)
CC?      OPEN (UNIT=LUN,FILE=ROUTINE(1:LR)//'.FOR',STATUS='NEW',
CC     &  CARRIAGECONTROL='LIST')
C
C ****  Write routine
C
      DO I =  1,NLINES
        WRITE(UNIT=LUN,FMT='(A)') LINE(I)(1:LLINE(I))
      ENDDO
C
C ****  Close file
C
      CLOSE(UNIT=LUN)
      CALL INTMSG(' Init Routine Done')
  999 RETURN
      END
