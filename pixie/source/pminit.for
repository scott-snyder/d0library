      FUNCTION PMINIT ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : INIT Interface Routine for PIXIE package
C-                         MUODIS
C-
C-   Read RCP file PX_MUODIS.RCP into memory.
C-
C-   Returned value  : TRUE
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  25-SEP-1990   Lupe Howell
C-   Updated  28-NOV-1990   Harrison B. Prosper
C-      Change name from MUINIT to PMINIT
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL PMINIT
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:PXMHTK.INC/LIST'
      INTEGER IER
      CHARACTER*(*) RCPFILE
      PARAMETER( RCPFILE = 'PX_MUODIS_RCP' )
      LOGICAL EZERROR
C----------------------------------------------------------------------
      LOGICAL OK, FIRST
      CHARACTER*1 CVAL, REM
      INTEGER TYP
      DATA FIRST/.TRUE./
      SAVE OK, FIRST
C----------------------------------------------------------------------
      IF( FIRST ) THEN
        FIRST = .FALSE.
        CALL INRCP (RCPFILE,IER)
C
C ****  Initialize menu
C
        CALL EZ_SETUP_COMPACK(RCPFILE,IER)
        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG
     &      ('PIXIE','PMINIT','Problem accessing PX_MUODIS_RCP','F')
        ENDIF
        OK = IER .EQ. 0
      ENDIF
C
C ****  Read in Muon Geometry file
C
C----      CALL XXXSTP('MUO_STPFILE',IER)
C---       OK = OK .AND. (IER.EQ.0)
C
C ****  Temporary common block initialization
C
      IF ( OK ) THEN
C
C ****  Pick correct RCP bank
C
        CALL EZPICK('PX_MUODIS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('PIXIE','PMINIT','Cannot find PX_MUODIS_RCP','F')
        ENDIF
C
        CALL EZ_GET_ARRAY('PXPARAMS','MUON DRAW MAGNET',
     &       1,PXMAG,CVAL,TYP,REM,IER)
        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG('PIXIE','PMINIT',
     &      'PXPARAMS NOT FOUND','W')
          GOTO 900
        ENDIF
C
        CALL EZ_GET_ARRAY('PXPARAMS','MUON LABEL CHAM',
     &       1,CHNUM,CVAL,TYP,REM,IER)
        CALL EZ_GET_ARRAY('PXPARAMS','MUON DRAW CELLS',
     &       1,PXMUD1,CVAL,TYP,REM,IER)
        CALL EZ_GET_ARRAY('PXPARAMS','MUON DRAW HITS',
     &       1,PXMUHT,CVAL,TYP,REM,IER)
        CALL EZ_GET_ARRAY('PXPARAMS','MUON DRAW TRACKS',
     &       1,PXMUTK,CVAL,TYP,REM,IER)
        CALL EZ_GET_ARRAY('PXPARAMS','MUON HITS ON TKS',
     &       1,PXMHTT,CVAL,TYP,REM,IER)
        CALL EZ_GET_ARRAY('PXPARAMS','MUON TIME DIV',
     &       1,PXMUTD,CVAL,TYP,REM,IER)
        CALL EZ_GET_ARRAY('PXPARAMS','MUON DRIFT TIME',
     &       1,PXMUDT,CVAL,TYP,REM,IER)
        CALL EZ_GET_ARRAY('PXPARAMS','MUON TRIG COUNTER',
     &       1,PXMUTC,CVAL,TYP,REM,IER)

        OK = OK .AND. (IER.EQ.0)
C
C ****  Reseting RCP file
C
  900   CONTINUE
        CALL EZRSET
      ENDIF
C
      PMINIT = OK
  999 RETURN
      END
