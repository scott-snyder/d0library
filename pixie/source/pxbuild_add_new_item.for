      SUBROUTINE PXBUILD_ADD_NEW_ITEM(IMENU,ISUBM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Adds a new screen item to the current list
C-   (common block) and to the PX_*_RCP file
C-
C-   Inputs  : IMENU   [I ]:   Menu index
C-             SUBM    [I ]:   Submenu  index
C-
C-   Outputs : None
C-
C-   Created   18-JUN-1991  Lupe Howell
C-   Updated   3-DEC-1991   Lupe Howell  Inserting new item to PX_*_RCp file
C-   Updated  24-JAN-1992   Lupe Howell  Update for SGI 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IMENU,ISUBM
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:PXBUILDCOM.INC'
      INCLUDE 'D0$OFFLINE_UTIL$GENERAL:VALUE.DEF'
      INCLUDE 'D0$PARAMS:PIXIE.DEF'
      INCLUDE 'D0$PARAMS:SRCP.DEF'
C----------------------------------------------------------------------
      INTEGER SCREEN_ELEMENTS
      PARAMETER( SCREEN_ELEMENTS = 22 )
C
      INTEGER IIVAL
      LOGICAL LLVAL
      REAL    RRVAL
      EQUIVALENCE  ( IIVAL,LLVAL,RRVAL )
C
      INTEGER I,J,START,IEND,ILEN,ICNUM,RECORD_COUNT,ALEN,ITYPE,IVAL
      INTEGER IPAR,PLEN,CLEN,KK,II,JJ,K,IER,LMENU,NITEMS
      INTEGER BLANK,IVALUEX,PORTNUM,MENU_CNT,NSCREEN
C
      REAL    VALUE,VALUEX

      CHARACTER*1 ZERO,TICK
      CHARACTER*2 CNUM
      CHARACTER*40 CVAL,PARAM_NAME,PARAM_TEMP,COMMAND_NAME
      CHARACTER*40 ACTION_ROUTINE,ARRAY_NAME,REMARK
      CHARACTER*40 NEW_ITEM
      CHARACTER*80 STRING,ITEM(MAXSAVE_P),RECORDS(MAXSCREENS)
      CHARACTER*80 MENU_ITEMS(10),MESS
C
      LOGICAL NEW,ONCE,EZERROR,LVALUEX,COMBINED
C----------------------------------------------------------------------
      RECORD_COUNT = 0
      MENU_CNT = 0
      TICK = CHAR(39)
      COMBINED = .FALSE.
C
C ****  Get menu item
C
      CALL OUTMSG('1')
   20 CALL GETPAR(1,' Enter the menu item name>','U',STRING)
      CALL SWORDS(STRING,START,IEND,ILEN)
      NEW_ITEM = STRING(START:IEND)
C
C ****  Check if the menu item is unique
C
      IF ( ILEN .GT. 0 ) THEN
        NEW = .TRUE.
        IF ( ACTION_COUNT(IMENU,ISUBM) .GT. 0 ) THEN
          I = 0
          DO WHILE ( NEW .AND.
     &                      (I .LT. ACTION_COUNT(IMENU,ISUBM)) )
            I = I + 1
            IF ( ACTION_ITEM(IMENU,ISUBM,I)(START:IEND) .EQ.
     &                       STRING(START:IEND) ) THEN
              NEW = .FALSE.
            ENDIF
          ENDDO
        ENDIF
        IF ( NEW ) THEN
C
C ****  Place logicals values for the menu
C
          MENU_CNT = MENU_CNT + 1
          MENU_ITEMS(MENU_CNT) = '     TRUE'
          MENU_CNT = MENU_CNT + 1
          MENU_ITEMS(MENU_CNT) = '    FALSE'
C
C ****  Saving menu item name
C
          MENU_CNT = MENU_CNT + 1
          MENU_ITEMS(MENU_CNT) = TICK//STRING(START:IEND)//TICK
C
C ****  Get command menu name
C
   30     CALL GETPAR
     &      (1,' Enter the command menu item name>','U',STRING)
          CALL SWORDS(STRING,START,IEND,ILEN)
C
C ****  Check if the command menu item is unique
C
          IF ( ILEN .GT. 0 ) THEN
            IF ( ACTION_COUNT(IMENU,ISUBM) .GT. 0 ) THEN
              I = 0
              DO WHILE ( NEW .AND.
     &                      (I .LT. ACTION_COUNT(IMENU,ISUBM)) )
                I = I + 1
                IF ( ACTION_COMMAND(IMENU,ISUBM,I)(START:IEND) .EQ.
     &                  STRING(START:IEND) ) THEN
                  NEW = .FALSE.
                ENDIF
              ENDDO
            ENDIF
            IF ( NEW ) THEN
              MENU_CNT = MENU_CNT + 1
              MENU_ITEMS(MENU_CNT) =
     &          TICK//STRING(START:IEND)//TICK
              COMMAND_NAME = STRING(START:IEND)
              CLEN = ILEN
C
C ****  Enter Remark
C
              CALL GETPAR
     &          (1,' Enter  remarks of this view>','C',STRING)
              CALL SWORDS(STRING,START,IEND,ILEN)
              IF ( ILEN .EQ. 0 ) THEN
                STRING = ' '
                START = 1
                IEND  = 1
              ENDIF
              MENU_CNT = MENU_CNT + 1
              MENU_ITEMS(MENU_CNT) =
     &          TICK//STRING(START:IEND)//TICK
C
C ****  Determine if a combined view or not
C
              CALL GETPAR(1,' Is this a combined view ?(Y/N)[N]>',
     &                'U',STRING)
              CALL SWORDS(STRING,START,IEND,ILEN)
C
C ****  COMBINED SCREEN
C
              IF ( STRING(1:1) .EQ. 'Y' ) THEN
                COMBINED = .TRUE.
                ACTION_ROUTINE = COMMAND_NAME
                CALL SWORDS(ACTION_ROUTINE,I,J,ALEN)
C
C ****  If a combined view, set a % at the end of the new name
C ****  and an undescore between words
C
                BLANK = INDEX(ACTION_ROUTINE(I:J),' ')
                DO  WHILE ( BLANK .NE. 0 )
                  ACTION_ROUTINE = ACTION_ROUTINE(1:BLANK-1)//'_'//
     &              ACTION_ROUTINE(BLANK+1:J)
                  BLANK = INDEX(ACTION_ROUTINE(I:J),' ')
                ENDDO
                IF ( ACTION_ROUTINE(J:J) .NE. '%' ) THEN
                  ACTION_ROUTINE = ACTION_ROUTINE(I:J)//'%'
                  J = J + 1
                  ALEN = ALEN + 1
                ENDIF
                MENU_ITEMS(4) = TICK//ACTION_ROUTINE(1:ALEN)//TICK
C
C ****  Get number of views in combined view
C
                MESS = ' Enter the number of views this '//
     &                 'combined view will have ?[1]>'
                CALL GETPAR(1,MESS,'U',CNUM)
                CALL WORD(CNUM,II,JJ,K)
                IF( K .EQ. 0 ) CNUM = '1'
                ICNUM = VALUE(CNUM,II,JJ,ITYPE)
                IF  ( ITYPE .NE. VTINT )
     &            GOTO 999              ! If non number entered out
C
C ****  Getting number of ports in combined view
C
                MESS = ' Enter the number of ports this '//
     &                 'combined view will have ?[1]>'
                CALL GETPAR(1,MESS,'U',CNUM)
                CALL WORD(CNUM,II,JJ,K)
                IF( K .EQ. 0 ) CNUM = '1'
                PORTNUM = VALUE(CNUM,II,JJ,ITYPE)
                IF (  ITYPE .NE. VTINT )
     &            GOTO 999               ! If non number entered out
C
C ****  Building the combined view with default values
C
                CALL PXBUILD_DEFAULT_COMBINED
     &            (ACTION_ROUTINE(1:ALEN),ICNUM,PORTNUM,
     &            RECORDS,RECORD_COUNT,IER)
                IF ( IER .NE. 0 ) GOTO 999
              ELSE
C
C ****  SINGLE SCREEN
C
                CALL GETPAR
     &            (1,' Please enter the name of the action routine>',
     &            'U',ACTION_ROUTINE)
                CALL SWORDS(ACTION_ROUTINE,I,J,ALEN)
                IF( ALEN .EQ. 0 ) GOTO 999
                CALL PXBUILD_DEFAULT_SCREEN
     &            (COMMAND_NAME,ACTION_ROUTINE(1:ALEN),
     &            RECORDS,RECORD_COUNT)
              ENDIF
C
C ****  Error message -- repeated command name
C
            ELSE
              MESS = ' The command name '//STRING(START:IEND)//
     &               ' was already added'
              CALL INTMSG(MESS)
              CALL INTMSG(' Please try another')
              GOTO 30
            ENDIF
C
C ****  Error message -- repeated item name
C
          ELSE
            MESS = ' The item name '//STRING(START:IEND)//
     &             ' was already added'
            CALL INTMSG(MESS)
            CALL INTMSG(' Please try another')
            GOTO 20
          ENDIF

C
C ****  Pick the RCP file
C
          CALL WORD(MENU_NAME(IMENU),I,J,LMENU)
          CALL EZPICK(MENU_NAME(IMENU)(I:LMENU)//'_RCP')
          IF ( EZERROR(IER) ) THEN
            CALL ERRMSG('INVALID RCP FILE','PXBUILD_ADD_NEW_ITEM',
     &        ,'Could not PICK '//MENU_NAME(IMENU)(1:LMENU),'W')
            GOTO 999
          ENDIF
C
C ****  Add the new screen names and remarks to the menu items
C
          CALL EZ_ADD_PARAM(
     &      ACTION_SUBMENU(IMENU,ISUBM),CVAL,1,-1,
     &      MENU_ITEMS,MENU_CNT,IER)
C
C ****  Add the new screen elements to the RCP file.
C ****  If the new screen is NOT a COMBINED screen
C ****  insert it in the PXSCREEN array.  Get the total number
C ****  of screens and increment it.
C
          IF ( .NOT. COMBINED ) THEN
            CALL EZ_GET_ARRAY
     &        ('PXSCREEN','NSCREEN',1,NSCREEN,CVAL,ITYPE,REMARK,IER)
C
C ****  Check if there is a PXSCREEN array.  If there isn't
C ****  create one inserting as the first element NSCREEN
C
            IF ( IER .EQ. EZS_PARAM_NOTFOUND ) THEN
              I = RECORD_COUNT + 1
              DO WHILE ( I .GT. 1 ) 
                RECORDS(I) = RECORDS(I-1)
                I = I - 1
              ENDDO
              RECORD_COUNT = RECORD_COUNT + 1
              ITYPE = VTINT
              CALL EZ_CVT_ELEMENT('NSCREEN',7,1,CVAL,
     &              0,'Total screens',13,ITYPE,RECORDS(1))
              CALL EZ_ADD_ARRAY('PXSCREEN',RECORDS,RECORD_COUNT,IER)
            ELSE
              CALL EZ_ADD_ELEMENT
     &          ('PXSCREEN','CAMZ',NSCREEN,1,RECORDS,RECORD_COUNT,IER)
              NSCREEN = NSCREEN + 1
              CALL EZ_SET_ARRAY('PXSCREEN','NSCREEN',NSCREEN,IER)
            ENDIF
          ELSE
C
C ****  If the new view is a COMBINED view add the combined
C ****  array to the RCP file
C
            ARRAY_NAME = ACTION_ROUTINE(1:ALEN)
            CALL EZ_ADD_ARRAY(ARRAY_NAME,RECORDS,RECORD_COUNT,IER)
          ENDIF
C
C ****  Add the new item to the menu list (common block)
C
          IF ( ACTION_COUNT(IMENU,ISUBM) .LT. MAXLIST ) THEN
            ACTION_COUNT(IMENU,ISUBM) =
     &           ACTION_COUNT(IMENU,ISUBM) + 1
          ENDIF
          ACTION_NAME(IMENU,ISUBM,
     &       ACTION_COUNT(IMENU,ISUBM)) = ACTION_ROUTINE(1:ALEN)
          ACTION_BANK(IMENU,ISUBM,
     &       ACTION_COUNT(IMENU,ISUBM)) =
     &       MENU_NAME(IMENU)(1:LMENU)//'_RCP'
          ACTION_ITEM(IMENU,ISUBM,ACTION_COUNT(IMENU,ISUBM)) =
     &       MENU_ITEMS(3)
          ACTION_COMMAND(IMENU,ISUBM,ACTION_COUNT(IMENU,ISUBM)) =
     &       MENU_ITEMS(4)
C
C ****  Restet RCP bank
C
          CALL EZRSET
        ENDIF
      ENDIF
  999 RETURN
      END

