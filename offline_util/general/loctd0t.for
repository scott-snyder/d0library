      FUNCTION LOCTD0T(LOCAL_TIME)
C----------------------------------------------------------------------
C-   Title: Local time to D0 time
C-   Purpose and Methods : Converts local time in D0 format to D0 time
C-                         by subtracting the timezone offset time. This
C-                         offset time
C-                         is stored in the logical name defined by the
C-                         parameter TZ_OFFSET.
C-
C-   Input  : Local time in local time format
C-   Output : D0_TIME in D0 time format
C-   Controls: None.
C-
C-   Created   9-JUN-1990   Stephen Adler
C-   Updated  10-DEC-1991   Herbert Greenlee
C-       UNIX compatible version
C-   Updated  18-Apr-1992   Herbert Greenlee
C-      Look first in process table, then d0 table
C-   Updated  18-MAY-1992   James T. Linnemann  ELN version 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LOCTD0T,LOCAL_TIME,BATAVIA_OFFSET
C&IF VAXVMS
      CHARACTER*5 LOGNAME
      INTEGER*4 INDEX,RET,LLOGNAME
      INTEGER*4 ITEM_LIST(4)
      INTEGER*2 WITEM_LIST(8)
      INTEGER*4 SYS$TRNLNM
      EXTERNAL SYS$TRNLNM
      EQUIVALENCE (ITEM_LIST,WITEM_LIST)
      INCLUDE '($LNMDEF)/NOLIST'
      INCLUDE '($SSDEF)/NOLIST'
      INCLUDE 'D0$PARAMS:BASE_DATE.PARAMS'
C&ELSE
C&      INTEGER D0_TZ_OFFSET
C&ENDIF
C----------------------------------------------------------------------
C
C&IF VAXVMS
      WITEM_LIST(1) = 5
      WITEM_LIST(2) = LNM$_STRING
      ITEM_LIST(2) = %LOC(LOGNAME)
      ITEM_LIST(3) = %LOC(LLOGNAME)
      ITEM_LIST(4) = 0

      INDEX = 0
      RET = SYS$TRNLNM(LNM$M_CASE_BLIND,
     &                        'LNM$PROCESS',
     &                        TZ_OFFSET,,
     &                        ITEM_LIST)
      IF(RET.NE.SS$_NORMAL)RET = SYS$TRNLNM(LNM$M_CASE_BLIND,
     &                        'LNM$D0',
     &                        TZ_OFFSET,,
     &                        ITEM_LIST)
      IF(RET.NE.SS$_NORMAL)THEN
        LOGNAME = '0'
        LLOGNAME = 1
      ENDIF

      READ(LOGNAME(1:LLOGNAME),FMT='(I2)') BATAVIA_OFFSET
C&ELSEIF VAXELN
C&      BATAVIA_OFFSET = 0          !current location of Level 2 is in Batavia
C&ELSE
C&      BATAVIA_OFFSET = D0_TZ_OFFSET()
C&ENDIF
      LOCTD0T = LOCAL_TIME - (BATAVIA_OFFSET * 3600)
  999 RETURN
      END
