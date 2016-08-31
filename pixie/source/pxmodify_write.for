      SUBROUTINE PXMODIFY_WRITE(CURRENT_RCP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Writes out the current RCP file to the local
C-   area
C-
C-   Inputs  : CURRENT_RCP [C*]:
C-   Outputs : None
C-
C-   Created   7-NOV-1991   Lupe Howell
C-   Updated  27-JAN-1992   Lupe Howell  Update for SGI 
C-   Updated  13-MAR-1992   Lupe Howell  Include DISPLAYED_ITEM_NAME 
C-   Updated  16-MAR-1992   Lupe Howell  Tidy up 
C-   Updated  19-MAR-1992   Lupe Howell  Tidy up 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) CURRENT_RCP
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:PXBUILDCOM.INC'
      INCLUDE 'D0$OFFLINE_UTIL$GENERAL:VALUE.DEF'
C----------------------------------------------------------------------
C
      INTEGER LUPE
      PARAMETER( LUPE = 500 )
C
      INTEGER IVAL
      LOGICAL LVAL
      REAL    RVAL
      EQUIVALENCE(IVAL,LVAL,RVAL)
C
      CHARACTER*23 DAY
      CHARACTER*40 FILE_NAME,PARAM
      CHARACTER*80 REMARK,MESS
      CHARACTER*132 BUFFER(LUPE),COMBINED(LUPE)
      CHARACTER*132 TEMP
      CHARACTER*132 CVAL
C
      INTEGER I,J,K,L,N,II,JJ,KK,CONTROL,CL,NREC,IER,TEMP_PTR
      INTEGER PTR,LPAR,LCVAL,LREM,TOTCOMB,ITYPE,IVALUE
C
      LOGICAL LAST,EZERROR,ACTIVE,MORE
C----------------------------------------------------------------------
      CALL EZPICK(CURRENT_RCP)
      IF ( EZERROR(IER) ) THEN
        GOTO 999
      ENDIF
C
C ****  Get date
C
      CALL LIB$DATE_TIME(DAY)
C
C ****  Open output file
C
      CALL WORD(CURRENT_RCP,I,J,CL)
      I = INDEX(CURRENT_RCP,'PX_') 
      IF ( I .EQ. 0 ) 
     &  I = 1
      J = INDEX(CURRENT_RCP,'_RCP') - 1 
      FILE_NAME = CURRENT_RCP(I:J)//'.RCP'
      CALL PXOPEN(LUN,FILE_NAME,'OL',IER)
      IF ( IER .NE. 0 ) GOTO 999
C
C ****  Write header
C
      TEMP = ' '
      TEMP = '\START '//CURRENT_RCP(1:CL)
      WRITE(UNIT=LUN,FMT='(A)')TEMP
      WRITE(UNIT=LUN,FMT='(A)')
     &  '\SIZE 500 200'
      WRITE(UNIT=LUN,FMT='(A)')
     &  '!---------------------------------------------------------'
      TEMP = ' '
      TEMP = '! Name:        '//FILE_NAME
      WRITE(UNIT=LUN,FMT='(A)')TEMP
      WRITE(UNIT=LUN,FMT='(A)')
     &  '! Purpose:     Control for PIXIE package '
      TEMP = ' '
      TEMP = '! Created:     '//DAY(1:11)//'  PXBUILD '
      WRITE(UNIT=LUN,FMT='(A)')TEMP
      WRITE(UNIT=LUN,FMT='(A)')
     &  '!---------------------------------------------------------'
      WRITE(UNIT=LUN,FMT='(A)')
     &  'ACTIVE       TRUE              ! Package ON/OFF switch'
C
C ****  Getting the DISPLAYED_ITEM_NAME
C
      CALL EZFETCH ('DISPLAYED_ITEM_NAME',LUPE,NREC,BUFFER,IER) 
      IF ( IER .EQ. 0 ) THEN
        WRITE(UNIT=LUN,FMT='(A)')BUFFER(1)
      ENDIF
C
C ****  Write out COMPACK menu definitions
C

      WRITE(UNIT=LUN,FMT='(A)')
     &  '!---------------------------------------------------------'
      WRITE(UNIT=LUN,FMT='(A)')
     &  '!   COMPACK SETUP'
      WRITE(UNIT=LUN,FMT='(A)')
     &  '!---------------------------------------------------------'
C
      WRITE(UNIT=LUN,FMT='(A)')
     &  '\ARRAY COMPACK_MENUS'
C
C ****  Writing Submenus
C
      CALL  EZFETCH ('COMPACK_MENUS',LUPE,NREC,BUFFER,IER)
      IF ( NREC .EQ. 0 ) THEN
        MESS = ' Problem writing file '//CURRENT_RCP
        CALL ERRMSG('SRCP','EZMERGE',MESS,'F')
        GOTO 999
      ENDIF
      IF( NREC .EQ. 1 ) THEN
        CALL WORD(BUFFER(1),I,J,N)
        TEMP = BUFFER(1)(J+1:132)
        CALL WORD(TEMP,I,J,N)
        BUFFER(1) = TEMP(I:J)
        BUFFER(2) = '\END'
        NREC = NREC + 1
        JJ = 1
      ELSEIF ( BUFFER(1)(1:6) .EQ. '\ARRAY' ) THEN
        JJ = 2
      ENDIF
      DO II = JJ, NREC
        CALL SWORDS(BUFFER(II),I,J,N)
        IF ( BUFFER(II)(I:J) .EQ. '\END' ) THEN ! don't leave blanks
          WRITE(UNIT=LUN,FMT='(A)')
     &      BUFFER(II)(I:J)
        ELSE
          TEMP = ' '
          TEMP = '    '//BUFFER(II)(I:J)
          WRITE(UNIT=LUN,FMT='(A)')TEMP
        ENDIF
      ENDDO
C
C ****  Write MENU definitions
C
      CONTROL = -1                      ! Trim tokens
      LAST    = .FALSE.
      ACTIVE  = .TRUE.
      II = JJ
      DO WHILE ( ACTIVE )
        I = 0
        J = 0
        CONTROL = -1                      ! Trim tokens
        LAST    = .FALSE.
        CALL GET_NEXT_TOKEN(BUFFER(II),' ',I,J,N,LAST,CONTROL)
        IER = 0
C
C ****  Writing array for for menu
C
        TEMP = ' '
        TEMP = '\ARRAY '//BUFFER(II)(I+1:J-1)
        WRITE(UNIT=LUN,FMT='(A)')TEMP
C
C ****  Getting the menu elements from array
C
        MORE = .TRUE.
        ITYPE = VTLOG
        PTR = 1
        TEMP_PTR = 1
        IER = 0
        DO WHILE ( MORE )
C
C ****  Writing the logival values
C
          DO WHILE ( ( ITYPE .EQ. VTLOG ) .AND.
     &               ( IER   .NE. 1     ) )
            CALL EZGET_NEXT_VALUE_TYPE
     &        (BUFFER(II)(I+1:J-1),IVALUE,CVAL,ITYPE,LVAL,IER,PTR)
            IF ( ITYPE .EQ. VTLOG ) THEN
              IF ( IVALUE .EQ. 0 ) THEN
                WRITE(UNIT=LUN,FMT='(A)')
     &            '    FALSE'
              ELSE
                WRITE(UNIT=LUN,FMT='(A)')
     &            '    TRUE'
              ENDIF
            ENDIF
          ENDDO
          IF ( IER .EQ. 0 ) THEN
C
C ****  Menu item and menu command
C
            TEMP = ' '
            TEMP = '    '''//CVAL(1:iVAL)//''''
            WRITE(UNIT=LUN,FMT='(A)')TEMP
            CALL EZGET_NEXT_VALUE_TYPE
     &        (BUFFER(II)(I+1:J-1),IVALUE,CVAL,ITYPE,LVAL,IER,PTR)
            TEMP = ' '
            TEMP = '    '''//CVAL(1:iVAL)//''''
            WRITE(UNIT=LUN,FMT='(A)')TEMP
C
C ****  Help
C
            DO WHILE( ( ITYPE .NE. VTLOG ) .AND. ( IER .NE. 1) )
              TEMP_PTR = PTR
              CALL EZGET_NEXT_VALUE_TYPE
     &          (BUFFER(II)(I+1:J-1),IVALUE,CVAL,ITYPE,LVAL,IER,PTR)
 1            IF(( ITYPE .NE. VTLOG ) .AND. ( iVAL .NE. 0 ))THEN
                TEMP = ' '
                TEMP = '    '''//CVAL(1:iVAL)//''''
                WRITE(UNIT=LUN,FMT='(A)')TEMP
              ENDIF
            ENDDO
            WRITE(UNIT=LUN,FMT='(A)')
          ENDIF
          MORE = ( IER .EQ. 0 )
          PTR = TEMP_PTR
        ENDDO
C
C ****  Write the '\END' of the array
C
        WRITE(UNIT=LUN,FMT='(A)')
     &          '\END'
C
C ****  If current token is the last move the buffre's pointer
C
        II = II + 1
        ACTIVE = ( II .LT. NREC )
      ENDDO
C
C ****  Write PXPARAMS
C
      CALL  EZFETCH ('PXPARAMS',LUPE,NREC,BUFFER,IER)
      IF ( IER .NE. 0 ) GOTO 100 ! IF no Parameters in this RCP file go on
      WRITE(UNIT=LUN,FMT='(A)')
     &    '!---------------------------------------------------------'
      WRITE(UNIT=LUN,FMT='(A)')
     &    '! Definition of Parameters'
      WRITE(UNIT=LUN,FMT='(A)')
     &    '!---------------------------------------------------------'
      WRITE(UNIT=LUN,FMT='(A)')
     &    '\ARRAY PXPARAMS'
C
C ****  Get triplet (Param-name, value, remark)
C
      PTR = 1
      IER = 0
      DO WHILE ( IER .EQ. 0 )
        CALL EZGET_NEXT_VALUE_TYPE('PXPARAMS',
     &          II,PARAM,JJ,LPAR,IER,PTR)
        CALL EZGET_NEXT_VALUE_TYPE('PXPARAMS',
     &          IVAL,CVAL,ITYPE,LCVAL,IER,PTR)
        CALL EZGET_NEXT_VALUE_TYPE('PXPARAMS',
     &          II,REMARK,JJ,LREM,IER,PTR)
C
C ****  Write out triplet
C
        CALL PXWRITE_PARAM(LUN,
     &                           PARAM,LPAR,
     &                           IVAL,
     &                           CVAL,LCVAL,
     &                           REMARK,LREM,
     &                           ITYPE)
      ENDDO
      WRITE(UNIT=LUN,FMT='(A)')
     &    '\END'
  100 CONTINUE
C
C ****  Getting the names of combined arrays if any
C
      PTR = 1
      I = 0
      TOTCOMB = 0
      DO WHILE ( PTR .NE. -1 )
        I = I + 1
        CALL EZGET_NEXT_NAME(BUFFER(I),PTR)
        CALL WORD(BUFFER(I),J,K,L)
        IF ( BUFFER(I)(L:L) .EQ. '%' ) THEN
          TOTCOMB = TOTCOMB + 1
          COMBINED(TOTCOMB) = BUFFER(I)
        ENDIF
      ENDDO
C
C ****  Write PXSCREEN
C
      CALL  EZFETCH ('PXSCREEN',LUPE,NREC,BUFFER,IER)
      IF ( IER .NE. 0 ) GOTO 200 ! IF no SCREENS in this RCP file go on
      WRITE(UNIT=LUN,FMT='(A)')
     &  '!---------------------------------------------------------'
      TEMP = ' '
      TEMP = 
     &  '! Definition of '//CURRENT_RCP(1:CL)//' Screens'
      WRITE(UNIT=LUN,FMT='(A)')TEMP
      WRITE(UNIT=LUN,FMT='(A)')
     &  '!---------------------------------------------------------'
      WRITE(UNIT=LUN,FMT='(A)')
     &  '\ARRAY PXSCREEN'
      PTR = 1
      IER = 0
C
C ****  Get triplet (Param-name, value, remark)
C
      DO WHILE ( IER .EQ. 0 )
        CALL EZGET_NEXT_VALUE_TYPE('PXSCREEN',
     &          II,PARAM,JJ,LPAR,IER,PTR)
        CALL EZGET_NEXT_VALUE_TYPE('PXSCREEN',
     &          IVAL,CVAL,ITYPE,LCVAL,IER,PTR)
        CALL EZGET_NEXT_VALUE_TYPE('PXSCREEN',
     &          II,REMARK,JJ,LREM,IER,PTR)
C
C ****  Write out triplet
C
        CALL PXWRITE_PARAM(LUN,
     &                           PARAM,LPAR,
     &                           IVAL,
     &                           CVAL,LCVAL,
     &                           REMARK,LREM,
     &                           ITYPE)
      ENDDO
      WRITE(UNIT=LUN,FMT='(A)')
     &    '\END'
  200 CONTINUE
C
C ****  Write Combined View Arrays
C
      IF ( TOTCOMB .EQ. 0 ) GOTO 800  ! Skip if no combined arrays
      WRITE(UNIT=LUN,FMT='(A)')
     &  '!---------------------------------------------------------'
      WRITE(UNIT=LUN,FMT='(A)')
     &  '!                  MultiView Commands'
      WRITE(UNIT=LUN,FMT='(A)')
     &  '!---------------------------------------------------------'
      DO II = 1, TOTCOMB
        CALL WORD(COMBINED(II),I,J,K)
        TEMP = ' '
        TEMP = '\ARRAY '//COMBINED(II)(I:J)
        WRITE(UNIT=LUN,FMT='(A)')TEMP
C
C ****  Get triplet (Param-name, value, remark)
C
        PTR = 1
        IER = 0
        DO WHILE ( IER .EQ. 0 )
          CALL EZGET_NEXT_VALUE_TYPE(COMBINED(II),
     &          KK,PARAM,JJ,LPAR,IER,PTR)
          CALL EZGET_NEXT_VALUE_TYPE(COMBINED(II),
     &          IVAL,CVAL,ITYPE,LCVAL,IER,PTR)
          CALL EZGET_NEXT_VALUE_TYPE(COMBINED(II),
     &          KK,REMARK,JJ,LREM,IER,PTR)
C
C ****  Write out triplet
C
          CALL PXWRITE_PARAM(LUN,
     &                           PARAM,LPAR,
     &                           IVAL,
     &                           CVAL,LCVAL,
     &                           REMARK,LREM,
     &                           ITYPE)
        ENDDO
        WRITE(UNIT=LUN,FMT='(A)')
     &      '\END'
        WRITE(UNIT=LUN,FMT='(A)') ' '
      ENDDO
C
C ****  Write STOP
C
      WRITE(UNIT=LUN,FMT='(A)')
     &  '\STOP'
C
C ****  Close file
C
      CLOSE(UNIT=LUN)
      CALL INTMSG(' RCP File Done')
  800 CALL EZRSET
C
  999 RETURN
      END
