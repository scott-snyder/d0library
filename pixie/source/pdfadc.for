      SUBROUTINE PDFADC
C===================================================================
C
C  Description:  Shows the FADC traces for a user chosen wire of
C  ============  the CDC's.
C
C
C  Author:
C  =======
C  Tami Kramer  Original Creation - May 29, 1987
C-   Updated  17-DEC-1987   Olivier Callot : Clean up
C-   Updated   8-JUL-1988   Olivier Callot  New graphic system
C
C====================================================================
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:GRAPHF77.INC'
      INCLUDE 'D0$INC:CDPARA.INC'
      INCLUDE 'D0$INC:PXPARA.INC'
C
      INTEGER I, J, K, IP, LON, IFIR, ISEG, ICL
      INTEGER ILAY, ISEC, IWIR, IMAX, IDMAX
      REAL    PHMAX, XWMIN, XWMAX, YWMIN, YWMAX
      REAL           VYMIN, VYMAX, VXMIN, VXMAX
      REAL    DPHMAX, DYWMIN, DYWMAX, VALMAX
      REAL    Y(LFADC), DY(LFADC), YWID
      INTEGER EVDATA(2*LFADC)
      CHARACTER*30 TEXTE
C
C  Executable Code:
C  ================
C
      CALL PUGETV( 'CDC LAYER', ILAY)
      CALL PUGETV( 'CDC SECTOR', ISEC)
      CALL PUGETV( 'CDC FADC', IWIR)
C
      CALL PUVPRT( -1., 1., .9, 1. )
      CALL JWINDO(-50.,50.,-3.,3.)
      CALL PUOPEN
      WRITE( TEXTE, 1000 ) ILAY, ISEC, IWIR
 1000 FORMAT(' Layer  ',I2,' Sector ',I2,' FADC   ',I2)
      CALL PXCOLR( 'CYA' )
      CALL JJUST( 2, 2)
      CALL JSIZE( 2., 4.)
      CALL PUSTRG( 0., 0., TEXTE )
      CALL PUCLOSE
C
      CALL CDUNPK(ILAY,ISEC,IWIR,EVDATA)
      CALL VZERO(  Y, LFADC)
      CALL VZERO( DY, LFADC)
      IP=1
   29 LON = EVDATA(IP)
      IF( LON.NE.0 ) THEN
        IFIR = EVDATA( IP+1)
        IP = IP + 2
        DO 30 K = 1,LON
          Y(IFIR+K) = FLOAT(EVDATA(IP))
          IP = IP + 1
          IF(K.GT.1)THEN
            DY(IFIR+K-1)=Y(IFIR+K)-Y(IFIR+K-1)
          ENDIF
   30   CONTINUE
        GOTO 29
      ENDIF
C
      CALL PUVPRT( -1., 1., .1, .8 )
      CALL PUHIST( LFADC, Y, 'MAG' )
C
      CALL PUVPRT( -1., 1., -.8, -.1 )
      CALL PUHIST( LFADC-1, DY, 'MAG' )
C
      RETURN
      END
