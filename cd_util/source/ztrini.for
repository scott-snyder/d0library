      FUNCTION ZTRINI()
C--------------------------------------------------------------
C
C  Initialization routine for ZTRAKS package.
C  Read control parameters.
C
C  Daria Zieminska May 1988; modified June 1989.
C
C-   Updated   4-FEB-1991   Daria Zieminska  Call VERINI 
C-   Updated  20-MAR-1991   Qizhong Li-Demarteau  added switches for 
C-                                   subdetectors and added flag for 
C-                                   ZTRAKS_RCP 
C-   Updated  10-JUL-1991   Qizhong Li-Demarteau  added EZERROR
C-   Updated  23-AUG-1991   Qizhong Li-Demarteau  make sure it is called
C-                                                only once
C-   Updated   1-NOV-1991   Qizhong Li-Demarteau  added call to EZLOC
C--------------------------------------------------------------
C
      IMPLICIT NONE
      LOGICAL ZTRINI
      LOGICAL ZTRDDF,OK,VTRINI,FTRINI,DTRINI,TRDINI,VERINI
      INTEGER IER, LRCP
      CHARACTER*(*) RCPFIL 
      PARAMETER( RCPFIL = 'ZTRAKS_RCP' )   ! Logical name of control file
      INCLUDE 'D0$INC:ZEBCOM.INC'
      LOGICAL VTXON, CDCON, FDCON, TRDON,CALL_VERTEX
      LOGICAL EZERROR, FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
C--------------------------------------------------------------
C
      IF (.NOT. FIRST) THEN
        ZTRINI = .TRUE.
        GOTO 999
      ELSE
        FIRST = .FALSE.
      ENDIF
C
      CALL EZLOC(RCPFIL,LRCP)
      OK = LRCP .GT. 0
      IF (.NOT. OK) THEN
        CALL INRCP(RCPFIL,IER)
        OK = IER .EQ. 0
        IF (.NOT. OK) CALL ERRMSG('ZTRAKS','ZTRINI',
     &    'Reading ZTRAKS_RCP failed','F')
      ENDIF
C
      CALL FLGBK('ZTRAKS_RCP',1)
      IF (OK) CALL FLGSET('ZTRAKS_RCP',.TRUE.)
C
      OK = ZTRDDF() .AND. OK       !  Get list of banks to dump
      CALL ZTRDRP(IER)
      IF (IER .NE. 0) OK = .FALSE.
      CALL EZPICK('ZTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('ZTRAKS','ZTRINI',
     &    'Unable to find bank ZTRAKS_RCP','F')
          GOTO 999
        ENDIF
      CALL EZGET('VTXON',VTXON,IER)
      CALL EZGET('CDCON',CDCON,IER)
      CALL EZGET('FDCON',FDCON,IER)
      CALL EZGET('TRDON',TRDON,IER)
      CALL EZGET('CALL_VERTEX',CALL_VERTEX,IER)
      CALL EZRSET
C
      IF (VTXON) OK = OK .AND. VTRINI()
      IF (CDCON) OK = OK .AND. DTRINI()
      IF (FDCON) OK = OK .AND. FTRINI()
      IF (TRDON) OK = OK .AND. TRDINI()
      IF (CALL_VERTEX) OK=OK.AND.VERINI()
      ZTRINI=OK 
C
  999 RETURN
      END
