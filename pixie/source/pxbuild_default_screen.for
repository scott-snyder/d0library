      SUBROUTINE PXBUILD_DEFAULT_SCREEN(NAME,ACTION,RECORDS,
     &  RECORD_COUNT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fills the values of screen parameters with the
C-   DI3000 defaults.
C-
C-   Inputs  : NAME       [C*]: Command name of the screen
C-             ACTION     [C*]: Name of the action routine
C-
C-   Outputs : RECORDS [C*(*)]: Array containing the records of the screen
C-                              values.
C-                              'Screen_param_name'  value  'Remark'
C-             RECOR_COUNT [I]: Total number of records
C-
C-   Created  26-JUL-1991   Lupe Howell
C-   Updated   3-DEC-1991   Lupe Howell  Inserting new item to PX_*_RCp file
C-   Updated  24-JAN-1992   Lupe Howell  Update SGI 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) NAME
      CHARACTER*(*) ACTION
      CHARACTER*(*) RECORDS(*)
      INTEGER RECORD_COUNT
C----------------------------------------------------------------------
      INCLUDE 'D0$OFFLINE_UTIL$GENERAL:VALUE.DEF'
C----------------------------------------------------------------------
      INTEGER SCREEN_ELEMENTS
      PARAMETER( SCREEN_ELEMENTS = 22 )
C
      CHARACTER*40 SINGLEPAR(SCREEN_ELEMENTS),DEF_VAL(SCREEN_ELEMENTS)
      CHARACTER*1 CVAL
      CHARACTER*80 MESS
      INTEGER IVAL,ITYPE,I,II,J,JJ,K,PLEN,DEF_TYPE(SCREEN_ELEMENTS)
      INTEGER IVALUEX,x,y,z
      REAL    VALUEX,VALUE
      LOGICAL LVALUEX
C
      INTEGER IIVAL
      LOGICAL LLVAL
      REAL    RRVAL
      EQUIVALENCE  ( IIVAL,LLVAL,RRVAL )
C----------------------------------------------------------------------
      DATA SINGLEPAR/'NAME','ACTIONS','PICKABLE','VIEW3D','KEEPCIRCLE',
     &  'VPORTXMIN','VPORTXMAX','VPORTYMIN','VPORTYMAX',
     &  'WINDOWXMIN','WINDOWXMAX','WINDOWYMIN','WINDOWYMAX',
     &  'VIEWREFX','VIEWREFY','VIEWREFZ','UPVECX','UPVECY',
     &  'UPVECZ','CAMX','CAMY','CAMZ'/
C
      DATA DEF_VAL/'0.',' ','FALSE','FALSE','TRUE','-1.0000',' 1.0000',
     &   '-1.0000',' 1.0000','-1.0000',' 1.0000','-1.0000',' 1.0000',
     &   ' 0.0000',' 0.0000',' 0.0000',' 0.0000',' 1.0000',' 0.0000',
     &   ' 0.0000',' 0.0000',' 1.0000'/
C
      DATA DEF_TYPE/0,0,VTLOG,VTLOG,VTLOG,VTREAL,VTREAL,VTREAL,VTREAL,
     &  VTREAL,VTREAL,VTREAL,VTREAL,VTREAL,VTREAL,VTREAL,VTREAL,VTREAL,
     &  VTREAL,VTREAL,VTREAL,VTREAL/
C----------------------------------------------------------------------
      CHARACTER*(*) VIEWNAME
      INTEGER VIEWNUM,PORTNUM
C
      INTEGER COMB_ELEMENTS
      PARAMETER( COMB_ELEMENTS = 18 )
C
      INTEGER VLEN,ILEN,PORTSIZE,IER,PL,AL,RL,ISCREEN,NSCREEN,IDX
      CHARACTER*80 STRING,CURR_PACKAGE,CURR_ACTION,CURR_RCP
      CHARACTER*40 COMBPAR(COMB_ELEMENTS)
      CHARACTER*32 PROMPT
      REAL    XMINPRT,XMAXPRT,YMINPRT,YMAXPRT,PORTVAL(COMB_ELEMENTS)
      REAL    TEMP
      LOGICAL FOUND
C----------------------------------------------------------------------
      DATA COMBPAR/'VIEW3D','VPORTXMIN','VPORTXMAX','VPORTYMIN',
     &  'VPORTYMAX','WINDOWXMIN','WINDOWXMAX','WINDOWYMIN','WINDOWYMAX',
     &  'VIEWREFX','VIEWREFY','VIEWREFZ','UPVECX','UPVECY',
     &  'UPVECZ','CAMX','CAMY','CAMZ'/
C
      DATA PORTVAL/0.0000,-1.0000,1.0000,-1.0000,1.0000,-1.0000,1.0000,
     &  -1.0000,1.0000,0.0000,0.0000,0.0000,0.0000,1.0000,0.0000,0.0000,
     &  0.0000,1.0000/
C----------------------------------------------------------------------
C
C ****  Setting up the NAME in the screen
C
      CALL SWORDS(NAME,I,J,K)
      ITYPE = K + 10
      RECORD_COUNT = RECORD_COUNT + 1
      CALL EZ_CVT_ELEMENT(SINGLEPAR(1),4,IVAL,NAME(I:J),
     &       K,NAME(I:J),K,ITYPE,RECORDS(RECORD_COUNT))
C
C ****  Setting up the ACTION in the screen
C
      CALL SWORDS(ACTION,I,J,K)
      ITYPE = K + 10
      RECORD_COUNT = RECORD_COUNT + 1
      CALL EZ_CVT_ELEMENT(SINGLEPAR(2),6,IVAL,ACTION(I:J),
     &       K,ACTION(I:J),K,ITYPE,RECORDS(RECORD_COUNT))
C
C
C ****  Filling the rest of the screen values with the defaulte values
C
      DO I = 3, SCREEN_ELEMENTS
        CALL WORD(SINGLEPAR(I),J,K,PLEN)
        RRVAL = VALUE(DEF_VAL(I),II,JJ,ITYPE)
        IF ( DEF_TYPE(I) .EQ. VTINT ) THEN     ! Integer
          IIVAL = IVALUEX
        ELSEIF( DEF_TYPE(I) .EQ. VTLOG ) THEN  ! Logical
          LLVAL = LVALUEX
        ENDIF
        RECORD_COUNT = RECORD_COUNT + 1
        CALL EZ_CVT_ELEMENT(SINGLEPAR(I),PLEN,IIVAL,CVAL,
     &       1,' ',1,DEF_TYPE(I),RECORDS(RECORD_COUNT))
      ENDDO
      RETURN
C
      ENTRY PXBUILD_DEFAULT_COMBINED(VIEWNAME,VIEWNUM,PORTNUM,
     &  RECORDS,RECORD_COUNT,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fills the values a combined view with DI3000
C-   defaults.  The window paramters will be set according to what values the
C-   original view had.  If for some reason we cannot get this values the
C-   default DI3000 window values will be set.
C-
C-   Inputs  :VIEWNAME[C*]:Name of the combined view
C-            VIEWNUM [I]: Number of views in the combied view
C-            PORTNUM [I]: Number of viewports in the combined view
C-
C-   Outputs :RECORDS[C*(*)]: Array containing the records of the screen
C-                            values.
C-                            'Screen_param_name'  value  'Remark'
C-            RECOR_COUNT[I]: Total number of records
C-            IER        [I]: -1 if package name or action name not entered
C-                             0 Ok
C-
C-
C-   Created   1-AUG-1991   Lupe Howell
C-
C----------------------------------------------------------------------
      IER = 0
C
C ****  Adding the %TITLE
C
      CALL WORD(VIEWNAME,I,J,VLEN)
      ITYPE = VLEN + 10
      RECORD_COUNT = RECORD_COUNT + 1
      CALL EZ_CVT_ELEMENT('%TITLE',6,IVAL,VIEWNAME,
     &     (VLEN-1),VIEWNAME,(VLEN-1),ITYPE,RECORDS(RECORD_COUNT))
C
C ****  Initializing port default values
C
      XMINPRT = -1.0000
      XMAXPRT =  1.0000
      YMINPRT = XMINPRT
      YMAXPRT = XMAXPRT
      PORTVAL(2) = XMINPRT
      PORTVAL(3) = XMAXPRT
      PORTVAL(4) = YMINPRT
      PORTVAL(5) = YMAXPRT
C
C ****  Building each combined view
C
      DO I = 1, VIEWNUM
C
C ****  Package
C

        WRITE(UNIT=CVAL,FMT='(I1)')I
        PROMPT = ' Enter Package name for View #'//CVAL//'>'
        CALL GETPAR(1,
     &    PROMPT,'U',STRING)
        CALL WORD(STRING,II,JJ,ILEN)
        IF( ILEN .EQ. 0 ) THEN
          IER = -1
          GOTO 999
        ENDIF
        CURR_PACKAGE = STRING(1:ILEN)
        PL = ILEN
        ITYPE = ILEN + 10
        RECORD_COUNT = RECORD_COUNT + 1
        CALL EZ_CVT_ELEMENT
     &              ('%PACKAGE',8,IVAL,STRING,ILEN,
     &              ' ',1,ITYPE,RECORDS(RECORD_COUNT))
C
C ****  ACTION
C
        PROMPT = ' Enter Action name for View #'//CVAL//'>'
        CALL GETPAR(1,PROMPT(1:31),'U',STRING)
        CALL SWORDS(STRING,II,JJ,ILEN)
        IF( ILEN .EQ. 0 ) THEN
          IER = -1
          GOTO 999
        ENDIF
        CURR_ACTION = STRING(1:ILEN)
        AL = ILEN
        ITYPE = ILEN + 10
        RECORD_COUNT = RECORD_COUNT + 1
        CALL EZ_CVT_ELEMENT
     &              ('%ACTION',7,IVAL,STRING(II:JJ),ILEN,
     &              ' ',1,ITYPE,RECORDS(RECORD_COUNT))

C
C ****  Calculating default port values for more than one port
C
        PORTSIZE = (XMAXPRT - XMINPRT)/2
        IF ( PORTNUM .GT. 1 ) THEN
          IF ( I .EQ. 3 ) THEN
            PORTVAL(4) = PORTVAL(4) + PORTSIZE
            PORTVAL(2) = XMINPRT
          ENDIF
          IF ( PORTNUM .GT. 2 )
     &      PORTVAL(5) = PORTVAL(4) + PORTSIZE
          IF ( I .EQ. 2 )
     &      PORTVAL(2) = PORTVAL(2) + PORTSIZE
          PORTVAL(3) = PORTVAL(2) + PORTSIZE
        ENDIF
C
C ****  Placing the '%SCREEN' element
C
        STRING = ' '
        ITYPE = 1 + 10
        RECORD_COUNT = RECORD_COUNT + 1
        CALL EZ_CVT_ELEMENT
     &              ('%SCREEN',7,IVAL,STRING,1,
     &              ' ',1,ITYPE,RECORDS(RECORD_COUNT))
C
C ****  Checking if the package set for the view is available to get
C ****  the window values.
C
        CALL EZTELL(CURR_RCP,RL)
        CALL EZRSET    ! Resting current RCP file
        FOUND = .TRUE.
        CURR_PACKAGE = 'PX_'//CURR_PACKAGE(1:PL)//'_RCP'
        CALL EZLOC(CURR_PACKAGE,IER)
        IF ( IER .EQ. 0 ) THEN  ! Read the package if was not read before
          MESS = 'D0$PIXIE:'//CURR_PACKAGE(1:PL+3)//'.RCP'
          CALL WORD(MESS,X,Y,Z)
          CALL INRCP(MESS(1:Z),IER)
          IF ( IER .NE. 0 ) THEN
            FOUND = .FALSE.
            GOTO 20
          ENDIF
        ENDIF
C
C ****  Get the screen number and the index of the action requested
C ****  for this view.  If the action was not found set FOUND to false
C
        CALL EZPICK(CURR_PACKAGE)
        CALL PU_GET_SCREEN_NUMBER
     &      (CURR_ACTION(1:AL),ISCREEN,NSCREEN,IER)
        IF ( IER .NE. 0 ) THEN
          FOUND = .FALSE.
        ELSE
          CALL PU_GOTO_SCREEN(ISCREEN,IDX) ! Get index of the screen
        ENDIF
        CALL EZRSET
C
C ****  Setting screen default values
C
   20   CVAL = ' '
        LLVAL = .FALSE.
        ITYPE = VTLOG
        RECORD_COUNT = RECORD_COUNT + 1
        CALL EZ_CVT_ELEMENT(COMBPAR(1),6,IIVAL,' ',
     &       1,' ',1,ITYPE,RECORDS(RECORD_COUNT))
        DO II = 2, COMB_ELEMENTS
          CALL WORD(COMBPAR(II),J,K,PLEN)
          ITYPE = VTREAL
          RRVAL = PORTVAL(II)
          RECORD_COUNT = RECORD_COUNT + 1
C
C ****  Checking For window values
C
          IF ( II .GE. 6 .AND. II .LE. 9 ) THEN
            IF ( FOUND ) THEN
              CALL EZPICK(CURR_PACKAGE)
              CALL PU_GET_SCREEN_PARAM
     &            (IDX,COMBPAR(II),TEMP,IER)
              CALL EZRSET
              IF ( IER .EQ. 0 ) THEN
                RRVAL = TEMP
              ENDIF
            ENDIF
          ENDIF
C
C ****  Setting the screen values in the recod array with
C ****  their vaule and remark
C
          CALL EZ_CVT_ELEMENT(COMBPAR(II),PLEN,IIVAL,CVAL,
     &       1,' ',1,ITYPE,RECORDS(RECORD_COUNT))
        ENDDO
      ENDDO
C
C ****  Setting the RCP file that was active 
C
      CALL EZPICK(CURR_RCP)
  999 RETURN
      END
