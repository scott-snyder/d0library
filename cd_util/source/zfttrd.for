      SUBROUTINE ZFTTRD(LFDCT,PASSTRD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : to determine if a FDC track passes TRD or not
C-
C-   Inputs  : LFDCT: FDC track address
C-   Outputs : PASSTRD: true if it passes TRD
C-
C-   Created   8-FEB-1992   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INTEGER LFDCT, IER
      REAL    RTRD, ZTRD, RFDC, ZFDC, RDIF, THETA
      LOGICAL PASSTRD, EZERROR, FIRST
      DATA    FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        CALL EZPICK('ZTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('ZTRAKS','ZFTTRD',
     &    'Unable to find bank ZTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('RTRD',RTRD,IER)
        CALL EZGET('ZTRD',ZTRD,IER)
        CALL EZRSET
      END IF
C
      RFDC = SQRT(Q(LFDCT+4)**2 + Q(LFDCT+5)**2)
      CALL FGETZ0(IQ(LFDCT-5),ZFDC)
      ZFDC = ABS(ZFDC)
      THETA = Q(LFDCT+22)
      RDIF = RFDC - ABS((ZFDC - ZTRD) * TAN(THETA))
      RDIF = ABS(RDIF)
      IF (RDIF .GT. RTRD) THEN
        PASSTRD = .TRUE.
      ELSE
        PASSTRD = .FALSE.
      ENDIF
C
  999 RETURN
      END
