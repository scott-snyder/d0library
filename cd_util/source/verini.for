      FUNCTION VERINI()
C--------------------------------------------------------------
C
C  Initialization routine for VERTEX package.
C  Read control parameters.
C
C  Daria Zieminska May 1988; modified June 1989.
C-   Updated  12-APR-1990   Qizhong Li-Demarteau  added a call to DTRINI 
C-   Updated  21-SEP-1990   Qizhong Li-Demarteau  added a call to FTRINI 
C-   Updated   3-FEB-1991   Daria Zieminska  remove calls to VTRINI,DTRINI,
C-                          FTRINI 
C-   Updated   3-APR-1991   Qizhong Li-Demarteau  added reading ZTRAKS.RCP
C-                                           for switches of subdetectors 
C-   Updated   1-NOV-1991   Qizhong Li-Demarteau  added call to EZLOC and
C-                           added calls to DTRINI, FTRINI and VTRINI again
C
C--------------------------------------------------------------
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      LOGICAL VERINI
      LOGICAL VERDDF, OK, FLGVAL
      LOGICAL EZERROR, OK1
      LOGICAL CDCON, FDCON, VTXON, DTRINI, FTRINI, VTRINI
      INTEGER IER, LRCP
      CHARACTER*(*) RCPFIL, RCPFILZ
      PARAMETER( RCPFIL = 'VERTEX_RCP' )   ! Logical name of control file
      PARAMETER( RCPFILZ = 'ZTRAKS_RCP' )
C--------------------------------------------------------------
C
      CALL EZLOC(RCPFIL,LRCP)
      OK = LRCP .GT. 0
      IF (.NOT. OK) THEN
        CALL INRCP(RCPFIL,IER)
        OK = IER .EQ. 0
        IF (.NOT. OK) CALL ERRMSG('VERTEX','VERINI',
     &    'Reading VERTEX_RCP failed','F')
      ENDIF
C
C   read ZTRAKS.RCP if it has not been read before
C
      IF (.NOT. FLGVAL('ZTRAKS_RCP')) THEN
        CALL EZLOC(RCPFILZ,LRCP)
        OK1 = LRCP .GT. 0
        IF (.NOT. OK1) THEN
          CALL INRCP(RCPFILZ,IER)
          OK1 = IER .EQ. 0
          IF (.NOT. OK1) CALL ERRMSG('VERTEX','VERINI',
     &      'Reading ZTRAKS_RCP failed','F')
        ENDIF
        IF (OK1) CALL FLGSET('ZTRAKS_RCP',.TRUE.)
        OK = OK1 .AND. OK
      ENDIF
      CALL EZPICK('ZTRAKS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('VERTEX','VERINI',
     &    'Unable to find bank ZTRAKS_RCP','W')
        GOTO 999
      ENDIF
      CALL EZGET('VTXON',VTXON,IER)
      CALL EZGET('CDCON',CDCON,IER)
      CALL EZGET('FDCON',FDCON,IER)
      CALL EZRSET
C
      IF (CDCON .AND. (.NOT. FLGVAL('DTRAKS_RCP'))) THEN
        OK = OK .AND. DTRINI()
      ENDIF
      IF (FDCON .AND. (.NOT. FLGVAL('FTRAKS_RCP'))) THEN
        OK = OK .AND. FTRINI()
      ENDIF
      IF (VTXON .AND. (.NOT. FLGVAL('VTRAKS_RCP'))) THEN
        OK = OK .AND. VTRINI()
      ENDIF
C
      OK=VERDDF().AND.OK       !  Get list of banks to dump
      CALL VERDRP(IER)
      IF(IER.NE.0) OK=.FALSE.
      VERINI=OK 
  999 RETURN
      END
