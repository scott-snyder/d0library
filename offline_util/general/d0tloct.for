      FUNCTION D0TLOCT(D0_TIME)
C----------------------------------------------------------------------
C-   Title: D0 time to Local time.
C-   Purpose and Methods : Converts D0 time to local time in D0 format
C-                         by adding the timezone offset time. This time
C-                         is stored in the logical name defined by the
C-                         parameter TZ_OFFSET.
C-
C-   Inputs  : D0_TIME in D0 time format
C-   Outputs : Local time in local time format
C-   Controls: None.
C-
C-   Created   9-JUN-1990   Stephen Adler
C-   Updated  11-DEC-1991   Herbert Greenlee
C-      UNIX compatible version
C-   Updated  18-Apr-1992   Herbert Greenlee
C-      Look first in process table, then d0 table
C-   Updated  18-MAY-1992   James T. Linnemann  ELN version 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER D0TLOCT,D0_TIME,BATAVIA_OFFSET
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
C&ELSE
C&      INTEGER D0_TZ_OFFSET
C&ENDIF
      INCLUDE 'D0$PARAMS:BASE_DATE.PARAMS'
C----------------------------------------------------------------------
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
      IF(RET.NE.SS$_NORMAL) THEN
        LOGNAME = '0'
        LLOGNAME = 1
      ENDIF

      READ(LOGNAME(1:LLOGNAME),FMT='(I2)') BATAVIA_OFFSET
C&ELSEIF VAXELN
C&      BATAVIA_OFFSET = 0
C&ELSE
C&      BATAVIA_OFFSET = D0_TZ_OFFSET()
C&ENDIF
      D0TLOCT = D0_TIME + (BATAVIA_OFFSET * 3600)

  999 RETURN
      END
