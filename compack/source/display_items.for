      SUBROUTINE DISPLAY_ITEMS(MAXITM,ITEMS,REM,TOPS,POSI)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Displays given items and allowds selection of
C-   one.  If no selection made POSI will return the value of 0
C-
C-   Inputs  : MAXITM  [I]: Maximum number of items to selecte from
C-             ITEMS[C(*)]: Array with names of the items
C-             REM [C*(*)]: Array with the remarks of each item for help
C-             TOPS   [C*]: Title to put on top of display
C-
C-   Outputs : POSI    [I]: Number of the selected file. If no selection 
C-                          this number is 0.
C-             
C-   Controls: None
C-
C-   Created   5-NOV-1991   Lupe Howell  Based in FNDFIL by Jan S. Hoftun
C-   Updated   3-JAN-1992   Lupe Howell  Temporary fix for the help 
C-   Updated   7-JAN-1992   Lupe Howell  Add array with remarks
C-   
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER POSI,MAXITM
      CHARACTER*(*) ITEMS(*)
      CHARACTER*(*) REM(*)
      CHARACTER*(*) TOPS
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
C----------------------------------------------------------------------
      CHARACTER*80 COMAND,UCOMAND,STRING
      INTEGER ISTAT,ITOP,IOLD,COLS
      INTEGER LIBBIG,LIBERA,LIBCUR,I,J,K,CURONF
      INTEGER POSO,LIBSCR,LOCSPA,TRULEN,NUM
      LOGICAL REGEN_NEEDED
      CHARACTER*132 BLANK,NEWTOP
      DATA BLANK/' '/
C----------------------------------------------------------------------
      ITOP=0
      IOLD=0
      POSI=1
      PF=0
C
C ****  Setting up the colomns for the display
C
  100 CONTINUE
      IF(PF.EQ.0) THEN
        IF(MAXITM.LT.5) THEN
          COLS=1
          LOCSPA=2
        ELSEIF(MAXITM.LT.16) THEN
          COLS=2
          LOCSPA=2
        ELSE
          COLS=2
          LOCSPA=1
        ENDIF
C
C ****  Display and wait for input in the FULL SCREEN Mode
C
        IF(FULSCR) THEN
   10     ISTAT=LIBERA(1,1)
          K=PBCOLS/4-TRULEN(TOPS)/2
          NEWTOP=BLANK(1:MAX0(K,1))//TOPS
          ISTAT=LIBBIG(NEWTOP,1,1,3)
          CALL PFLABL('DO','HELP','MENU','BACK')
          CALL LINES1(POSI,ITEMS,MAXITM,ITOP,IOLD,COLS,LOCSPA)
  200     CONTINUE
          POSO=POSI
C
C ****  Get input from the user FULSCREEN MODE
C
          CALL CURSO1(PF, POSI, MAXITM, COLS, REGEN_NEEDED)
          IF (REGEN_NEEDED) THEN  ! Regenerate the menu display if needed
            ISTAT=LIBBIG(NEWTOP,1,1,3)
            CALL PFLABL('DO','HELP','MENU','BACK')
            CALL LINES1(POSI,ITEMS,MAXITM,ITOP,IOLD,COLS,LOCSPA)
          ENDIF
          IF(POSI.NE.POSO.AND.PF.EQ.0) THEN
            CALL LINES(POSO,POSI,ITEMS,MAXITM,ITOP,IOLD,COLS,LOCSPA)
          ENDIF
          IF(PF.EQ.0) GOTO 200
          ISTAT = CURONF(0)   ! make sure cursor is on again.
          IF( PF .EQ. 4 ) POSI = 0
          IF( PF .EQ. 3 ) GOTO 10 ! Redraw the menu if MENU requested
          IF( (PF .EQ. 2) .AND. (POSO .EQ. POSI) ) THEN ! Display HELP 
            CALL SWORDS(REM(POSI),I,K,J)
            CALL INTMSG(REM(POSI)(1:J))
            PF=0
            GOTO 10
          ENDIF
C
C ****  Display and wait for input in in LINE Mode
C
        ELSE
   20     IF(TRMFLG) THEN
            CALL LINES0(ITEMS,MAXITM,COLS)
          ENDIF
          COMAND = ' '
          UCOMAND = ' '
          CALL CURLIN('{name, #, HELP (#), MENU, BACK}',
     *                     PF,POSI,MAXITM,COMAND)
          CALL UPCASE(COMAND,UCOMAND)
          CALL SWORDS(UCOMAND,I,J,K)
          IF ( UCOMAND .EQ. 'MENU' ) GOTO 20 ! Display the menu again if
                                            ! MENU
          IF ( UCOMAND(I:4) .EQ. 'HELP' ) THEN
            STRING = UCOMAND(5:80)
            CALL SWORDS(STRING,I,J,K) ! Get the number for help
            READ(STRING(I:J),30)NUM
            CALL SWORDS(REM(NUM),I,K,J)
            CALL INTMSG(REM(NUM)(1:K))
            GOTO 20
          ENDIF
        ENDIF
      ENDIF
   30 FORMAT(I<K>)
   40 FORMAT(A2)
      RETURN
      END
