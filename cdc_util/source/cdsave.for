      SUBROUTINE CDSAVE(LUNOUT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Generate a output file with all calibration
C-                         parameters
C-
C-   Inputs  : LUNOUT [I] :  Output unit, may be for printing
C-   Outputs : on unit LUNOUT
C-
C-   Created  12-AUG-1987   Olivier Callot
C-   Updated  25-NOV-1987   Olivier Callot
C-   Updated  30-MAR-1989   Qizhong Li-Demarteau  take away control 
C-                                      parameter dumpping and use SRCP
C-   Updated  29-MAY-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CDPARA.INC'
      INCLUDE 'D0$LINKS:IZDALH.LINK'
      INTEGER I, J, K, LAYER, SECTOR, LUNOUT
      INTEGER JDPDL, JDGNL, JDTML, IPD, IGN, ITM
      INTEGER JDALL, JDALS, IAL
      INTEGER MAXLAY, MAXSEC, ERR
      INTEGER IER
      LOGICAL EZERROR
      LOGICAL FIRST
C
      SAVE FIRST
      DATA   FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('DTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('DTRAKS','CDSAVE',
     &    'Unable to find bank DTRAKS_RCP','W')
          GOTO 999
        ENDIF
        CALL EZGET('MAXLAY',MAXLAY,ERR)
        CALL EZGET('MAXSEC',MAXSEC,ERR)
        CALL EZRSET
      ENDIF
C
C ****  Write all infos for each sector
C
      LDALH = LC( LSCDC - IZDALH )
      DO 100 LAYER = 0, MAXLAY
        JDPDL = LC( LDPDH-LAYER-1 )
        JDGNL = LC( LDGNH-LAYER-1 )
        JDTML = LC( LDTMH-LAYER-1 )
        JDALL = LC( LDALH-LAYER-1 )
        DO 110 SECTOR = 0, MAXSEC
          WRITE( LUNOUT, 3000 ) LAYER, SECTOR
 3000     FORMAT(' LAYER====SECTOR',10('=')/I5,I10)
          WRITE( LUNOUT, 3100 )
 3100     FORMAT(' adc| pedestal|  sig ped|   gain  |',
     &      '   T0    |velocity |     x0  |     y0  |     z0  |',
     &      '   Cphi  |   Sphi  |')
          JDALS = LC( JDALL-SECTOR-1 )
          DO 150 I = 0, MXFADC
            IPD = JDPDL + (SECTOR*IC(JDPDL+4)+I)*IC(JDPDL+3) + 4
            IGN = JDGNL + (SECTOR*IC(JDGNL+4)+I)*IC(JDGNL+3) + 4
            IF ( I .GE. IC(JDALS+5) ) THEN
              ITM = LC( LDTMH-LAYER-5 )
              ITM = ITM + ( SECTOR*IC(ITM+4)+I-7)*IC(ITM+3) + 4
              WRITE( LUNOUT, 3200 ) I,C(IPD+1),C(IPD+2),C(IGN+1),
     &                              C(ITM+1),C(ITM+2)
            ELSE
              ITM = JDTML + (SECTOR*IC(JDTML+4)+I)*IC(JDTML+3) + 4
              IAL = JDALS + I*IC(JDALS+6) + 6
              WRITE( LUNOUT, 3200 ) I,C(IPD+1),C(IPD+2),C(IGN+1),
     &                              C(ITM+1),C(ITM+2),(C(IAL+K),K=1,3),
     &                              C(JDALS+3),C(JDALS+4)
            ENDIF
 3200       FORMAT(I5,2F10.3,F10.6,F10.2,F10.6,3F10.4,2F10.6)
  150     CONTINUE
  110   CONTINUE
  100 CONTINUE
      WRITE( LUNOUT, 1600 )
 1600 FORMAT(' ',30('-'),' END OF FILE ',30('-'))
C
  999 RETURN
      END
