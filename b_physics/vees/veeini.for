      FUNCTION VEEINI
C--------------------------------------------------------------
C
C  Initialization routine for VEES package.
C  Read control parameters.
C
C  9-JUL-1990 Daria Zieminska 
C
C--------------------------------------------------------------
C
      IMPLICIT NONE
      LOGICAL VEEINI,VEEDDF,OK,FLGVAL
      INTEGER IER ,IERSUM
      CHARACTER*(*) RCPFIL, RCPFILZ,RCPFILD,RCPFILF,RCPFILV
      PARAMETER( RCPFILZ = 'ZTRAKS_RCP' )
      PARAMETER( RCPFILD = 'DTRAKS_RCP' )
      PARAMETER( RCPFILF = 'FTRAKS_RCP' )
      PARAMETER( RCPFILV = 'VTRAKS_RCP' )
      PARAMETER( RCPFIL = 'VEES_RCP' )   ! Logical name of control file
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      CALL INRCP (RCPFIL,IER)  ! Read parameter file into an SRCP bank
      OK=IER.EQ.0
      IERSUM=0
      IF (.NOT. FLGVAL('ZTRAKS_RCP')) CALL INRCP(RCPFILZ,IER)
      IERSUM=IERSUM+IER
      IF (.NOT. FLGVAL('DTRAKS_RCP')) CALL INRCP(RCPFILD,IER)
      IERSUM=IERSUM+IER
      IF (.NOT. FLGVAL('FTRAKS_RCP')) CALL INRCP(RCPFILF,IER)
      IERSUM=IERSUM+IER
      IF (.NOT. FLGVAL('VTRAKS_RCP')) CALL INRCP(RCPFILV,IER)
      IERSUM=IERSUM+IER
      OK = (IERSUM .EQ. 0) .AND. OK
      IF (OK) THEN
        CALL FLGSET('ZTRAKS_RCP',.TRUE.)
        CALL FLGSET('DTRAKS_RCP',.TRUE.)
        CALL FLGSET('FTRAKS_RCP',.TRUE.)
        CALL FLGSET('VTRAKS_RCP',.TRUE.)
      END IF
      OK=VEEDDF().AND.OK       !  Get list of banks to dump
      IF(IER.NE.0) OK=.FALSE.
      VEEINI=OK 
  999 RETURN
      END
