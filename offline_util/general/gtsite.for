      FUNCTION GTSITE()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns either the Cluster alias name or the
C-   node name, which ever is found frist.
C-
C-   Inputs  : None
C-   Outputs : Node name.
C-   Controls: None
C-
C-   Created  10-JUL-1990   Stephen Adler
C-   Updated   9-Mar-1992   Herbert Greenlee
C-      UNIX version (returns nodename)
C-   Updated  18-MAY-1992   James T. Linnemann  ELN version 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*8 GTSITE
C----------------------------------------------------------------------
C&IF VAXVMS
      CHARACTER*15 LOGNAME
      INTEGER*4 LLOGNAME
      CHARACTER*8 NLOGNAME
      INTEGER*4 RET,LNLOGNAME,I
      INTEGER*4 ITEM_LIST(4),STRINDX,ENDINDX
      INTEGER*2 WITEM_LIST(8)
      INTEGER*4 SYS$TRNLNM
      EXTERNAL SYS$TRNLNM
      EQUIVALENCE (ITEM_LIST,WITEM_LIST)
      INCLUDE '($LNMDEF)/NOLIST'
      INCLUDE '($SSDEF)/NOLIST'
C----------------------------------------------------------------------
C<<
      LOGNAME = '????????'
      LLOGNAME = 8
C<<
      WITEM_LIST(1) = LEN(LOGNAME)
      WITEM_LIST(2) = LNM$_STRING
      ITEM_LIST(2) = %LOC(LOGNAME)
      ITEM_LIST(3) = %LOC(LLOGNAME)
      ITEM_LIST(4) = 0
C<<
      RET = SYS$TRNLNM(LNM$M_CASE_BLIND,
     &                        'LNM$SYSTEM_TABLE',
     &                        'SYS$CLUSTER_NODE',,
     &                        ITEM_LIST)
      IF (RET .NE. SS$_NORMAL) THEN
        RET = SYS$TRNLNM(LNM$M_CASE_BLIND,
     &                        'LNM$SYSTEM_TABLE',
     &                        'SYS$NODE',,
     &                        ITEM_LIST)
        IF (RET .NE. SS$_NORMAL) THEN
          LOGNAME = '????????'
          LLOGNAME = 8
        ENDIF
      ENDIF
C<<
      STRINDX = INDEX(LOGNAME(1:LLOGNAME),'_')+1
      ENDINDX = INDEX(LOGNAME(1:LLOGNAME),'::')-1
      IF (ENDINDX.EQ.-1) ENDINDX = LLOGNAME
      LNLOGNAME = ENDINDX - STRINDX + 1
      IF (LNLOGNAME.LE.8) THEN
        NLOGNAME = LOGNAME(STRINDX:ENDINDX)
        DO I = LNLOGNAME+1, 8
          NLOGNAME(I:I) = ' '
        END DO
      ELSE
        NLOGNAME = '????????'
        LNLOGNAME = 8
      ENDIF
      GTSITE = NLOGNAME
C&ELSEIF VAXELN
C&      GTSITE = 'D0_L2_ELN'
C&ELSE
C&      GTSITE = ' '
C&      CALL D0_NODENAME(GTSITE)
C&ENDIF
  999 RETURN
      END
