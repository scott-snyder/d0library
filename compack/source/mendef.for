      SUBROUTINE MENDEF
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Add menu items for a specific level.
C-
C-   Inputs :  None
C-   Outputs : None
C-
C-   Created   22-MAY-89   by MENU_MAKER
C-   Updated  26-FEB-1991   Boaz Klima, Harrison B. Prosper
C-       Do it only once
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*64 MENNAM
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL MENNEW('MENUDEF')
        CALL MENADD('MENUDEF',.TRUE.,
     &   'Spawn subprocess',
     &   '@SUBSET',
     &   ' This command will exit the program into a spawned'//
     &   ' process to let you issue other DCL commands.  A b'//
     &   'ox will appear in the upper right hand corner of t'//
     &   'he screen to remind you that you''''re in a subpro'//
     &   'cess. To exit from the subprocess you type LOGOUT.'//
     &   ' This will take you back to the main menu.  NOTE: '//
     &   'Not all DCL commands will work from this spawned p'//
     &   'rocess, in particular the ones which spawn process'//
     &   'es themselves like EDEBUG/LOAD and the ones which '//
     &   'were defined by SET COMMAND.                      '//
     &   ' '   )
        CALL MENADD('MENUDEF',.FALSE.,
     &   'Change Columns in Menu',
     &   'CHCOLS',
     &   ' This command will allow you to change the layout '//
     &   'of the menu. You will be prompted for the number o'//
     &   'f columns to use in the display. Currently only 1 '//
     &   'or 2 columns will work well.  When in 2 column, fu'//
     &   'll-screen mode, the left-right arrow keys works as'//
     &   ' well as the up-down ones.                        '//
     &   ' '   )
        CALL MENADD('MENUDEF',.FALSE.,
     &   'Change Spacing in Menu',
     &   'CHSPAC',
     &   ' This command will ask for the new spacing of menu'//
     &   ' items in full screen mode. 1, 2 or 3 are the curr'//
     &   'ently allowed values. 2 is the default.           '//
     &   ' '   )
        CALL MENADD('MENUDEF',.FALSE.,
     &   'Directory for Com-files',
     &   'CALL SETDIR',
     &   '   This command will ask for the name of the defau'//
     &   'lt directory to use for running command files. In '//
     &   'line mode this default may be overriden by enterin'//
     &   'g a directory specification on the command line.  '//
     &   ' '   )
        CALL MENADD('MENUDEF',.TRUE.,
     &   'Run Command File',
     &   'CALL RUNCOM',
     &   ' This command will prompt for the name of a comman'//
     &   'd file to be run. The file must contain commands s'//
     &   'pelled out enough to be unique and the required in'//
     &   'put for each command as separate lines.  See SET U'//
     &   'P COMMAND FILE for help on how to create such a fi'//
     &   'le.  If the file is not found or cannot be opened '//
     &   'for some other reason, a message will be typed, in'//
     &   'cluding the IOSTAT number.                        '//
     &   ' '   )
        CALL MENADD('MENUDEF',.FALSE.,
     &   'Set up Command File',
     &   'CALL RUNSET',
     &   ' This command will prompt for the name of a comman'//
     &   'd file to be set up.  The program will then go int'//
     &   'o a normal menu mode, but instead of actually doin'//
     &   'g the commands it will save each input line in the'//
     &   ' command file.  You finish this input mode by usin'//
     &   'g EXIT as usual.                                  '//
     &   ' '   )
        CALL MENADD('MENUDEF',.FALSE.,
     &   'Start Logging to File',
     &   'CALL RUNLOG',
     &   ' This command will prompt for the name of a comman'//
     &   'd file for the  commands to be saved in.  Each com'//
     &   'mand will then be saved in this file which may be '//
     &   'used as a command file later.  You finish this log'//
     &   'ging mode by using the END LOGGING command.       '//
     &   ' '   )
        CALL MENADD('MENUDEF',.FALSE.,
     &   'Stop Logging to File',
     &   'CALL ENDLOG',
     &   ' This command will end the logging of commands to '//
     &   'a file.                                           '//
     &   ' '   )
        CALL MENADD('MENUDEF',.FALSE.,
     &   'Add Menu Item',
     &   'CALL ADDITM',
     &   ' This command will let you add a VMS one-line comm'//
     &   'and as a menu item. You will be prompted for the n'//
     &   'ame of the command. The name will appear in the me'//
     &   'nu list. You will then be prompted for the command'//
     &   ' to be executed. The command MUST be uderstood by '//
     &   'VMS. A complicated command may be performed by usi'//
     &   'ng a command file.  The same restrictions as under'//
     &   ' SPAWN above are valid here as well.              '//
     &   ' '   )
        CALL MENADD('MENUDEF',.FALSE.,
     &   'Split Screen',
     &   'CALL SPLIT',
     &   ' This command will split the screen in two halves,'//
     &   ' one for messages and one for menu interactions. U'//
     &   'se the routine INTMSG to send messages to the uppe'//
     &   'r half of the screen.  Use SINGLE SCREEN to go bac'//
     &   'k to a one screen display again.                  '//
     &   ' '   )
        CALL MENADD('MENUDEF',.TRUE.,
     &   'Single Screen',
     &   'CALL ENDSPLIT',
     &   ' This command will restore a single screen display'//
     &   '.                                                 '//
     &   ' '   )
        CALL MENADD('MENUDEF',.FALSE.,
     &   'Status Screen',
     &   'CALL SPLSTA',
     &   ' This command will put the status screen on top of'//
     &   ' the display. It will ask for how many lines to us'//
     &   'e, the default 3 gives one line of text plus two b'//
     &   'order lines.  Use the routine STAMSG to send messa'//
     &   'ges to the status part of the screen.  Use NOSTATU'//
     &   'S SCREEN to go back to a one screen display again.'//
     &   ' '   )
        CALL MENADD('MENUDEF',.TRUE.,
     &   'Nostatus Screen',
     &   'CALL ENDSTA',
     &   ' This command will remove the status part of the s'//
     &   'creen display.                                    '//
     &   ' '   )
        CALL MENADD('MENUDEF',.TRUE.,
     &   'Top Level Menu',
     &   'CALL GOTOP',
     &   ' This command will bypass the menu tree and go dir'//
     &   'ectly back to the top level menu.                 '//
     &   ' '   )
        CALL MENADD('MENUDEF',.TRUE.,
     &   'Refresh Screen',
     &   'CALL REFRESH',
     &   ' This command will refresh the screen to take away'//
     &   ' any broadcast messages etc. which may have appear'//
     &   'ed.                                               '//
     &   ' '   )
      ENDIF
      RETURN
      END
