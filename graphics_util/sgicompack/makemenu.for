      INTEGER FUNCTION MAKEMENU(NI,MENU,TITLE)
C----------------------------------------------------------------------
C-
C-  Purpose and Methods: Main control of menu set up display and loop 
C-  until a command has been entered. 
C-  Makes menu windows, fills then, and services pointer returning
C-  0 for exit and otherwise the index of the item chosen.
C-  It handles the HELP by letting the user pick the item he will like help.
C-  This routine checks for redraw of the graphcis window.  If a redraw
C-  event is found it will exit to redraw the display
C-
C-  Inputs:  NI       [I]: Total number of elements in MENU array
C-           MENU [C*(*)]: Array containing the menu items
C-           TITLE [C(*)]: Title for the menu 
C-
C-  Output: None
C-
C-    Created : 25-JUN-1991  Michael Shupe
C-    Updated : 13-AUG-1992  Lupe Howell Redraw the menu if damage
C-    Updated : 15-JAN-1993  Lupe Howell Interrupt 
C-    Updated : 09-FEB-1993  Lupe Howell Help display
C-                           
C----------------------------------------------------------------------
C
      CHARACTER*(*) MENU(*),TITLE
      INTEGER NI
C----------------------------------------------------------------------
      INCLUDE 'D0$GRAPHICS_UTIL$SGICOMPACK:MAINPACK.INC'
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
      INCLUDE 'fgl.h'
      INCLUDE 'fdevice.h'
C
      CHARACTER*2048 HLPINF
      INTEGER*4 IWOLD,IWDID,IDEV,GDEV
      INTEGER TEMP,NT,IYBASE,IYLINE,IWDINT,ICHO,MMENU,IIVDID,LRWDO
      INTEGER*2 QVAL,GVAL
      INTEGER ISCRX,ISCRY,IDY,MYWD,IDYL,MYHT,IY0,IX0,IX1,IY1
      INTEGER IXMARG,IRX,IRY,LWINDO,NITM,MAXWID,I,NL,MYL
C
      LOGICAL FIRST,DIFLG,MICON,SPNMEN
      LOGICAL TASK,HELP
C
      DATA FIRST/.TRUE./
      DATA DIFLG/.FALSE./
      DATA MICON/.FALSE./
      DATA ISCRX,ISCRY/1280,1024/
      SAVE FIRST,DIFLG
C
C  COMMON CONNECTING TO DI3GL TO ALLOW FOR ADDED FEATURES IN THIS
C  MENU
      COMMON/DI3CTL/LWINDO,LRWNDO
C----------------------------------------------------------------------
      HELP = .FALSE.
      ISCRX=GETGDE(GDXPMA)
      ISCRY=GETGDE(GDYPMA)
C
C *** Tiny icon window in upper right-hand corner in order to allow
C *** menu wait mode
C
      IF( MICON ) THEN
        IDY=20
        MYWD=60
        IDYL=.2*IDY
        MYHT=1.5*IDY
        IY0=ISCRY-MYHT-32
        IX0=ISCRX-MYWD-10
        IX1=IX0+MYWD
        IY1=IY0+MYHT
        IXMARG=IX0+10
        CALL PREFPO(IX0,IX1,IY0,IY1)
        IWDID=WINOPE('Icon',4)
        CALL WINTIT('Icon',4)
        CALL ORTHO2(FLOAT(IX0),FLOAT(IX1),FLOAT(IY0),FLOAT(IY1))
        CALL QDEVIC(LEFTMO)
        CALL QRESET()
        CALL WINSET(IWDID)
        CALL FRONTB(.TRUE.)
        CALL COLOR(WHITE)
        CALL CLEAR
        CALL COLOR(RED)
        CALL CMOV2(FLOAT(IXMARG),FLOAT(IY0+IDYL))
        CALL CHARST('MENU',4)
   44   CONTINUE
        IDEV=QREAD(QVAL)                  
        IF(IDEV.NE.LEFTMO) GO TO 44
        IF(QVAL.NE.1) GO TO 44
        IRX=GETVAL(MOUSEX)
        IRY=GETVAL(MOUSEY)
        CALL WINCLO(IWDID)
      ENDIF
C
C *** LWINDO will be non-zero only if DI3GL has been linking in along
C *** with the present routine
C
      IF( FIRST ) THEN
        IF(LWINDO.NE.0) THEN
           LWINDO=1                     
           DIFLG=.TRUE.
        ENDIF
        FIRST = .FALSE.
      ENDIF
C
      IWOLD=WINGET()
C
C *** Add items to end of non-interrupt menu
C
      IF(IRNOW.EQ.0) THEN
        NITM=NI+2
      ELSE
        NITM=NI
      ENDIF
C
C *** Add items to end of menu for DI3GL
C
      IF(DIFLG) THEN
        NITM=NITM+2
      ENDIF
C
C *** Find widest item
C
      MAXWID=0
      DO 21 I=1,NI
        NL=MYL(MENU(I))
C        KWIDTH=STRWID(MENU(I))
        IF(NL.GE.MAXWID)MAXWID=NL
   21 CONTINUE
C
C *** Define the size of the menu window
C
      IDY=20         
      MYWD=MAXWID*10
      IDYL=.2*IDY
      MYHT=NITM*IDY+2.*IDY
      IY0=ISCRY-MYHT-32
      IX0=ISCRX-MYWD-10
      IX1=IX0+MYWD
      IY1=IY0+MYHT
      IXMARG=IX0+10
      CALL PREFPO(IX0,IX1,IY0,IY1)
C
C *** Open the menu window
C
      NT=MYL(TITLE)
      IWDID=WINOPE(TITLE,NT)
      CALL WINTIT(TITLE,NT)
      CALL ORTHO2(FLOAT(IX0),FLOAT(IX1),FLOAT(IY0),FLOAT(IY1))
C
C *** Set up the mouse
C
      CALL QDEVIC(LEFTMO)
      CALL QRESET()
C
C *** Set current window
C
      CALL WINSET(IWDID)
      CALL FRONTB(.TRUE.)
 22   CALL COLOR(WHITE)
      CALL CLEAR
C
C *** Write menu items and item box boundaries
C
      DO 10 I=1,NITM
        CALL COLOR(RED)
        IYBASE=IY0+IDY*FLOAT(NITM-I+1)       
        IYLINE=IYBASE+IDYL                   
        CALL CMOV2(FLOAT(IXMARG),FLOAT(IYLINE))
        NL=MYL(MENU(I))
        IF(I.LE.NI) THEN
          CALL CHARST(MENU(I),NL)
        ELSE
          IF(IRNOW.EQ.0) THEN
            IF(I.EQ.NI+1) THEN
              CALL CHARST('Help',4)
            ELSEIF(I.EQ.NI+2) THEN
              CALL CHARST('Exit',4)
            ELSE
              IF(DIFLG) THEN
                IF(I.EQ.NI+3) THEN
                  CALL COLOR(BLACK)
                  CALL CHARST('Iconize Menu',12)
                ELSEIF(I.EQ.NI+4) THEN
                  CALL COLOR(BLACK)
                  CALL CHARST('Spin Menu',9)
                ENDIF
              ENDIF
            ENDIF
          ELSE
            IF(DIFLG) THEN
              IF(I.EQ.NI+1) THEN
                CALL COLOR(BLACK)
                CALL CHARST('Iconize Menu',12)
              ELSE
                CALL COLOR(BLACK)
                CALL CHARST('Spin Menu',9)
              ENDIF
            ENDIF
          ENDIF
        ENDIF
C
C *** Box each item for pointer and remember boundaries.
C
        CALL COLOR(BLUE)
        CALL MOVE2(FLOAT(IX0),FLOAT(IYBASE))
        CALL DRAW2(FLOAT(ISCRX),FLOAT(IYBASE))
   10 CONTINUE
C
C *** Check for interrupt menu.
C
      IF(IRNOW.GT.0) THEN
        IWDINT=IWDID
        IF(IWOLD.GT.0) CALL WINSET(IWOLD)
        RETURN
      ENDIF
C
C *** Handle non-interrupt menu.
C
   88 CONTINUE
      IDEV=QREAD(QVAL)
C
C *** Check for window damage in the graphics window and the menu 
C *** Setting the graphics window checking in the queue for redraw
C *** and if it is found set the redraw flag
C *** MAKEMENU will return the value of 0 and exit
C
      IF( IDEV .EQ. REDRAW ) THEN
	CALL RESHAP()
        GOTO 22
      ENDIF                  
C
      IF(IDEV.NE.LEFTMO) GO TO 88
      IF(QVAL.NE.1) GO TO 88
      IRY=GETVAL(MOUSEY)
      ICHO=NITM-((IRY-IY0)/IDY)+1
C
C *** Analyse the choice if in the menu boundry
C
      IF((ICHO.GT.0).AND.(ICHO.LE.NITM)) THEN
        MMENU=ICHO
C
C *** If in HELP mode and an item selected 
C *** display the help
C
        IF ( HELP .AND. MMENU.LE. NI) THEN
          CALL HLPROC(MMENU)
          HELP = .FALSE.
        ELSE
C
C *** Correct the menu choice if a non menu item 
C *** selected
C
          IF(ICHO.GT.NI) MMENU=-(ICHO-NI)        
C
C *** If EXIT was selected set output value to 0
C
          IF ( MMENU .EQ. -2 ) THEN
	    MMENU =  0
          ELSEIF ( MMENU .EQ. -1 ) THEN
C
C *** If the HELP choice was selected display 
C *** prompt for the user to select the menu
C *** item he/she wants to get help
C
            CALL STAMSG(' SELECT THE MENU ITEM TO DISPLAY HELP',.TRUE.)
            HELP = .TRUE.
            GO TO 88
          ENDIF
          MAKEMENU=MMENU
	  TEMP = MMENU
          IF(DIFLG) THEN
           IF(IRNOW.GT.0) THEN
              IF(MMENU.EQ.-1) THEN
                GO TO 87
              ELSEIF(MMENU.EQ.-2) THEN
                GO TO 86
              ENDIF
            ELSE
              IF(MMENU.EQ.-3) THEN
                GO TO 87
              ELSEIF(MMENU.EQ.-4) THEN
                GO TO 86
              ENDIF
            ENDIF
          ENDIF
          CALL WINCLO(IWDID)
          IF(IWOLD.GT.0) CALL WINSET(IWOLD)
          GO TO 999
        ENDIF
      ENDIF
      GO TO 88
C
C *** Toggle Icon and Window
C
   87 IF(MICON) THEN
        MICON=.FALSE.
      ELSE
        MICON=.TRUE.
      ENDIF                         
      GO TO 88
C
C *** Toggle Spin Menu
C
   86 IF(SPNMEN) THEN
        SPNMEN=.FALSE.
      ELSE
        SPNMEN=.TRUE.
      ENDIF
      GO TO 88
C-----------------------------------------------------------
C     ENTRY DELE_DISP
C  Cleanup menu.  Also cancellation entry for interrupt mode.
C-----------------------------------------------------------
      ENTRY DELE_DISP(IIVDID)
C
C *** If the window ID is 0 or less means close
C *** interrupt window
C
      IF ( IIVDID .LT. 0 ) THEN
        CALL WINCLO(IWDINT)
      ELSE
C
C *** Close the window using the given ID 
C
        CALL WINCLO(IIVDID)
      ENDIF
      GOTO 999
C------------------------------------------------------------
C     ENTRY ITASK_QUEUE
C  Entry to look at event queue in pseudo-interrupt mode
C  If the interrupt is activated returns task = .true.
C------------------------------------------------------------
      ENTRY ITASK_QUEUE(TASK)
   90 CONTINUE
      DO WHILE ( .TRUE. )
C
C *** Check on the event queue, if there is something
C *** Read it otherwise exit
C 
        IF ( QTEST() .EQ. 0 ) THEN
          TASK = .FALSE.
          GOTO 999
        ENDIF
C
C *** Read the event queue
C
        IDEV=QREAD(QVAL)
C
C *** Check if the event queue read left most 
C *** If is not LFTMO exit 
C       
        IF( (IDEV.NE.LEFTMO).OR.(QVAL.NE.1) ) THEN
          TASK = .FALSE.
          GOTO 999
        ENDIF 
C
C *** Check the value of the selction with the mouse
C *** To make sure it selected the stop sequencia display menu
C     
        IRY=GETVAL(MOUSEY)
        ICHO=NITM-((IRY-IY0)/IDY)+1
        IF((ICHO.GT.0).AND.(ICHO.LE.NITM)) THEN
          TASK = .TRUE.
        ELSE
          TASK =.FALSE.
        ENDIF
        GOTO 999
      ENDDO
 999  RETURN
      END
