      SUBROUTINE DBARC_LIST(MENU,UNIT,PATH,NRUN,LRUN,LCRATE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print a list of all key information in the path
C-
C-   Inputs  : MENU     determines if header is printed
C-             UNIT     unit number for printing
C-             PATH     DBL3 path name
C-             NRUN     number of runs
C-             LRUN     listof runs
C-             LCRATE   list of crates
C-   Outputs :
C-   Controls:
C-
C-   Created   27-FEB-1990   J.Green
C-   Created   25-MAR-1991   S. Abachi  modified and renamed from
C-                                        dbclb_list
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) PATH
      INTEGER IRUNS,IRUNE,NRUN,LRUN(*),LCRATE(*)
      CHARACTER*17 CALTYP
      CHARACTER*6 PARSE(4)
      INTEGER ITEM, IDATE, DAY, MONTH, YEAR
      INTEGER UNIT
      CHARACTER*1 TYPARR(3)   / 'I','I','I' /
      CHARACTER*6 PROARR(3)   / ' DD > ',' MM > ',' YY > ' /
      CHARACTER*40 COMAND
      LOGICAL GOTOP, MENU
C
      CALL DBCLB_PARSE ( PATH, PARSE )
C
      IF(MENU) THEN
        COMAND = ' '
      ELSE
        COMAND = 'GO'
      ENDIF
    1 CONTINUE
      IF (COMAND.NE.'EXIT'.AND..NOT.GOTOP()) THEN
        IF(MENU) CALL MENUDO('ARCHIVING OF THE DATABASE',
     &                        'ENARCVLIST',COMAND)
        IF(COMAND.EQ.'GO') THEN
          CALL DBARC_LIST_OUT(UNIT,PATH,NRUN,LRUN,LCRATE)
          COMAND = 'EXIT'               ! go back after printing
        ELSEIF(COMAND.EQ.'START') THEN
          CALL GETPAR ( 1, ' Enter first accepted start validity > ',
     &                  'I', ITEM )
          CALL DBARC_LIST_SET ( ITEM, 'SV' )
        ELSEIF(COMAND.EQ.'LAST') THEN
          CALL GETPAR ( 1, ' Enter last accepted end validity >', 'I',
     &                  ITEM )
          CALL DBARC_LIST_SET ( ITEM, 'LV' )
        ELSEIF(COMAND.EQ.'FST_CREATE') THEN
          CALL INTMSG (
     &      ' Enter first accepted creation date > ' )
          CALL GETPAR ( 3, PROARR, TYPARR,
     &                  DAY, MONTH, YEAR )
          IDATE = YEAR*10000 + MONTH*100 + DAY
          CALL DBPKTS ( IDATE, 0, ITEM )
          CALL DBARC_LIST_SET ( ITEM, 'SC' )
        ELSEIF(COMAND.EQ.'LST_CREATE') THEN
          CALL INTMSG (
     &      ' Enter Last accepted creation date  ' )
          CALL GETPAR ( 3, PROARR, TYPARR,
     &                  DAY, MONTH, YEAR )
          IDATE = YEAR*10000 + MONTH*100 + DAY
          CALL DBPKTS ( IDATE, 235959, ITEM )
          CALL DBARC_LIST_SET ( ITEM, 'LC' )
        ELSEIF(COMAND.EQ.'FST_INSERT') THEN
          CALL INTMSG (
     &      ' Enter first accepted insertion date  ' )
          CALL GETPAR ( 3, PROARR, TYPARR,
     &                  DAY, MONTH, YEAR )
          IDATE = YEAR*10000 + MONTH*100 + DAY
          CALL DBPKTM ( IDATE, 0, ITEM )
          CALL DBARC_LIST_SET ( ITEM, 'SI' )
        ELSEIF(COMAND.EQ.'LST_INSERT') THEN
          CALL INTMSG (
     &      ' Enter last accepted insertion date  ' )
          CALL GETPAR ( 3, PROARR, TYPARR,
     &                  DAY, MONTH, YEAR )
          IDATE = YEAR*10000 + MONTH*100 + DAY
          CALL DBPKTM ( IDATE, 235959, ITEM )
          CALL DBARC_LIST_SET( ITEM, 'LI' )
        ELSEIF(COMAND.EQ.'CRATE') THEN
          IF ( PARSE(2)(1:1) .EQ. 'M' ) THEN
            CALL GETPAR ( 1,' Enter desired module number >','I',ITEM )
          ELSE
            CALL GETPAR ( 1,' Enter desired crate number >', 'I',ITEM )
          ENDIF
          CALL DBARC_LIST_SET ( ITEM, 'CR' )
        ELSEIF(COMAND.EQ.'CALIB_TYPE') THEN
          CALL GETPAR ( 1,' Enter desired CALTYPE >','C',CALTYP )
          CALL UCTOH(CALTYP(1:4),ITEM,4,4)
          CALL DBARC_LIST_SET ( ITEM, 'CT' )
        ELSEIF(COMAND.EQ.'CLEAR') THEN
          CALL DBARC_LIST_SET ( ITEM, 'CL' )
        ELSEIF(COMAND.NE.'EXIT'.AND.COMAND.NE.'BLANK') THEN
          CALL OUTMSG(' No ACTION defined for that command'//CHAR(7))
        ENDIF
        GOTO 1
      ENDIF
C
  999 RETURN
      END
