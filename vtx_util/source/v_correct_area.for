      SUBROUTINE V_CORRECT_AREA(LAYER,WIRE,DRIFT,AREA)
C---------------------------------------------------------------------
C-
C-      Purpose : Correct Pulse Area Based on Drift Distance
C-
C-      Input : Layer
C-              Wire
C-              Drift : Drift Distance ( from sense plane )
C-              Area : Uncorrected Area
C-
C-      Output : Area : Corrected Area
C-
C-      Created  11-AUG-1992   Myungyun Pang
C-   Updated   2-NOV-1993   Ed Oltman   USE NEW FORMAT FOR CORRECTION
C-   Updated  11-NOV-1993   Liang-Ping Chen Do not apply correction to MC data,
C-                          (VGNL in MC STP is dummy, C(KPVGNL+6) is not 
C-                          consistently filled for the application here).
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
c I/O:
      INTEGER LAYER, WIRE
      REAL    DRIFT, AREA
c Locals:
      INTEGER ICATG,KPVGNL,IX
c Externals:
      INTEGER GZVGNL
C----------------------------------------------------------------------
      IF ( IQ(LHEAD + 1) .GT. 1000 ) GOTO 999    !  MC data
C
      KPVGNL = GZVGNL(LAYER)
      IX = C(KPVGNL+6)*ABS(DRIFT)
      IF (IX .GT. IC(KPVGNL+8+WIRE)) THEN
        AREA = AREA/C(KPVGNL+16+WIRE)
        GO TO 999
      ENDIF
      IF ( WIRE .EQ. 0 .OR. WIRE .EQ. 7) THEN
        AREA = AREA/C(KPVGNL + 24 + IX)
      ELSE
        AREA = AREA/C(KPVGNL + 24 + IC(KPVGNL+15) + 1 + IX)
      ENDIF
  999 RETURN
      END
