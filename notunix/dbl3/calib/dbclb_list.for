      SUBROUTINE DBCLB_LIST ( PATH )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print a list of all key information in the path
C-
C-   Inputs  : PATH     DBL3 path name
C-   Outputs : 
C-   Controls: 
C-
C-   Created   27-FEB-1990   J.Green 
C-
C----------------------------------------------------------------------
      IMPLICIT      NONE
      INCLUDE      'D0$PARAMS:CALIB.DEF'
      INCLUDE      'D0$INC:ZEBSTP.INC'
      INCLUDE      'D0$INC:DBSTP.INC'
      INCLUDE      'D0$INC:LKCALIB.INC'
      INCLUDE      'D0$INC:QUEST.INC'
      CHARACTER*(*) PATH
      CHARACTER*6   PARSE(4)
      CHARACTER*32  FILNAM
      CHARACTER*1   CHSF                ! answer questions
      CHARACTER*80  MSGSTR
      INTEGER       UNIT, SVUNIT        ! UNIT NUMBERS
      INTEGER       IERR, IOS
      INTEGER       ITEM, IDATE
      INTEGER       DAY, MONTH, YEAR
      CHARACTER*1   TYPARR(3)   / 'I','I','I' /
      CHARACTER*6   PROARR(3)   / ' DD > ',' MM > ',' YY > ' /
      INTEGER         IDEBDB, IDISDB, IDIVDB, IHKYDB, IKDRDB, IONODB    
     +              , KOFSDB, KOFUDB, LBDADB, LBFXDB, LBFYDB, LBKYDB    
     +              , LBNODB, LFIXDB, LSAVDB, LTOPDB, LPRTDB, NTOPDB    
      COMMON /DBUSER/ IDEBDB, IDISDB, IDIVDB, IHKYDB, IKDRDB, IONODB    
     +              , KOFSDB, KOFUDB, LBDADB, LBFXDB, LBFYDB, LBKYDB    
     +              , LBNODB, LFIXDB, LSAVDB, LTOPDB, LPRTDB, NTOPDB    
      CHARACTER*40  COMAND
      LOGICAL       GOTOP
      LOGICAL       FIRST    /.TRUE./
C----------------------------------------------------------------------
      CALL DBCLB_PARSE ( PATH, PARSE )
      SVUNIT = LPRTDB
      CALL GETPAR ( 1, ' Dump to screen or file ? S/[F]> ', 'U', CHSF )
      IF ( CHSF .EQ.'S' ) THEN
        LPRTDB = 6
      ELSE
        CALL GTUNIT ( 171, UNIT, IERR )
        LPRTDB = UNIT
        FILNAM = PARSE(2)//'_'//PARSE(3)//'_'//PARSE(4)//'.LIS'
        OPEN ( UNIT=UNIT, STATUS='NEW', FORM='FORMATTED', FILE=FILNAM,  
     &         IOSTAT = IOS )
        IF ( IOS .NE. 0 ) THEN
          WRITE ( MSGSTR, 1000 ) FILNAM, UNIT
 1000     FORMAT ( ' Error opening file = ', A, ' Unit =', I )
          CALL INTMSG ( MSGSTR )
          GO TO 999
        ELSE
          WRITE ( MSGSTR, 1001 ) FILNAM
 1001     FORMAT ( ' Output to file ', A )
          CALL INTMSG ( MSGSTR )
        ENDIF
        
      ENDIF
C
      COMAND=' '
    1 CONTINUE
      IF (COMAND.NE.'EXIT'.AND..NOT.GOTOP()) THEN
        CALL MENUDO('LIST DATABASE INFORMATION ','DBL3_LIST',COMAND)
        IF(COMAND.EQ.'WRITE') THEN
          CALL DBCLB_LIST_SET ( ITEM, 'FI' )
          CALL DBPRIN ( PATH, 'K' )
          COMAND = 'EXIT'               ! go back after printing
        ELSEIF(COMAND.EQ.'START') THEN
          CALL GETPAR ( 1, ' Enter first accepted start validity > ', 
     &                  'I', ITEM ) 
          CALL DBCLB_LIST_SET ( ITEM, 'SV' )
        ELSEIF(COMAND.EQ.'LAST') THEN
          CALL GETPAR ( 1, ' Enter last accepted end validity >', 'I',
     &                  ITEM ) 
          CALL DBCLB_LIST_SET ( ITEM, 'LV' )
        ELSEIF(COMAND.EQ.'FST_CREATE') THEN
          CALL INTMSG ( 
     &      ' Enter first accepted creation date > ' )
          CALL GETPAR ( 3, PROARR, TYPARR,
     &                  DAY, MONTH, YEAR )
          IDATE = YEAR*10000 + MONTH*100 + DAY
          CALL DBPKTS ( IDATE, 0, ITEM )
          CALL DBCLB_LIST_SET ( ITEM, 'SC' )
        ELSEIF(COMAND.EQ.'LST_CREATE') THEN
          CALL INTMSG ( 
     &      ' Enter Last accepted creation date  ' )
          CALL GETPAR ( 3, PROARR, TYPARR,
     &                  DAY, MONTH, YEAR )
          IDATE = YEAR*10000 + MONTH*100 + DAY
          CALL DBPKTS ( IDATE, 235959, ITEM )
          CALL DBCLB_LIST_SET ( ITEM, 'LC' )
        ELSEIF(COMAND.EQ.'FST_INSERT') THEN
          CALL INTMSG ( 
     &      ' Enter first accepted insertion date  ' )
          CALL GETPAR ( 3, PROARR, TYPARR,
     &                  DAY, MONTH, YEAR )
          IDATE = YEAR*10000 + MONTH*100 + DAY
          CALL DBPKTM ( IDATE, 0, ITEM )
          CALL DBCLB_LIST_SET ( ITEM, 'SI' )
        ELSEIF(COMAND.EQ.'LST_INSERT') THEN
          CALL INTMSG ( 
     &      ' Enter last accepted insertion date  ' )
          CALL GETPAR ( 3, PROARR, TYPARR,
     &                  DAY, MONTH, YEAR )
          IDATE = YEAR*10000 + MONTH*100 + DAY
          CALL DBPKTM ( IDATE, 235959, ITEM )
          CALL DBCLB_LIST_SET ( ITEM, 'LI' )
        ELSEIF(COMAND.EQ.'CRATE') THEN
          IF ( PARSE(2)(1:1) .EQ. 'M' ) THEN
            CALL GETPAR ( 1,' Enter desired module number >','I',ITEM ) 
          ELSE
            CALL GETPAR ( 1,' Enter desired crate number >', 'I',ITEM )
          ENDIF
          CALL DBCLB_LIST_SET ( ITEM, 'CR' )
        ELSEIF(COMAND.EQ.'CLEAR') THEN
          CALL DBCLB_LIST_SET ( ITEM, 'CL' )
        ELSEIF(COMAND.NE.'EXIT'.AND.COMAND.NE.'BLANK') THEN
          CALL OUTMSG('0No ACTION defined for that command'//CHAR(7))
        ENDIF
        GOTO 1
      ENDIF


      IF ( CHSF .NE. 'S' ) THEN
        CLOSE (UNIT=UNIT)
        CALL RLUNIT ( 171, UNIT, IERR )
      ENDIF
      LPRTDB = SVUNIT
C
  999 RETURN
      END
