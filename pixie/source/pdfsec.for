      SUBROUTINE PDFSEC
C===================================================================
C
C  Description:  Shows the FADC traces for a user chosen sector of
C  ============  the CDC's.
C
C
C-   Created  29-MAY-1987   Tami Kramer
C-   Updated  17-DEC-1987   Olivier Callot  : Clean up
C    Updated   9-JAN-1990   Lupe Howell
C
C====================================================================
C
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:GRAPHF77.INC'
      INCLUDE 'D0$INC:CDPARA.INC'
      INCLUDE 'D0$INC:PXPARA.INC'
C
C  Local Declarations:
C  ====================
C
      INTEGER I, J, K, KK, IP, LON, IFIR, ISEG
      INTEGER LAY, SECT, IWIR, IMAX, IVIEW
      INTEGER UPSID, NUFADC
      CHARACTER*3 COLOR
      REAL    VXMIN, VXMAX, VYMIN, VYMAX
      REAL    PHMAX, Y(LFADC),YWID, YSIZ, YSEP
      INTEGER EVDATA(2*LFADC)
      CHARACTER*22 TITLE
      DATA UPSID/0/
C
C  Executable Code:
C  ================
C
      CALL PUGETV('CDC LAYER', LAY )
      CALL PUGETV('CDC SECTOR', SECT )
C
      CALL PUVPRT( -1., 1., .9, 1. )
      CALL JWINDO(-50.,50.,-3.,3.)
      WRITE( TITLE, 1000 ) LAY, SECT
 1000 FORMAT( 'CDC Layer',I2,' Sector ',I2 )
      CALL PUOPEN
      CALL PXCOLR('CYA')
      CALL JJUST( 2, 2)
      CALL JSIZE( 2., 4.)
      CALL PUSTRG( 0., 0., TITLE )
      CALL PUCLOSE
      CALL JWINDO(-100.,100.,-100.,100.)

      VXMIN =  -1.
      VXMAX =  1.
      VYMAX =  .88
      YWID  =  1.8  / ( NBSENS + NBDELY )
      YSEP  =  .1 * YWID
      YSIZ  = YWID - YSEP

      DO 20 IWIR = 1, NBSENS+2*NBDELY
        UPSID = 1
        IF ( IWIR .EQ. 1 ) THEN
          NUFADC= NBSENS
          VYMIN = VYMAX - YSIZ/2
        ELSEIF ( IWIR .EQ. 2 ) THEN
          NUFADC= NBSENS + 1
          UPSID = -1
          VYMAX = VYMIN
          VYMIN = VYMAX - YSIZ/2
        ELSEIF ( IWIR.GT.2 .AND. IWIR.LE.NBSENS+2 ) THEN
          NUFADC = IWIR-3
          VYMAX = VYMIN - YSEP
          VYMIN = VYMAX - YSIZ
        ELSEIF ( IWIR .EQ. NBSENS+3 ) THEN
          NUFADC = NBSENS + 2
          VYMAX = VYMIN - YSEP
          VYMIN = VYMAX - YSIZ/2
        ELSEIF ( IWIR .EQ. NBSENS+4 ) THEN
          NUFADC = NBSENS + 3
          UPSID  = -1
          VYMAX = VYMIN
          VYMIN = VYMAX - YSIZ/2
        ENDIF
        CALL CDUNPK( LAY, SECT, NUFADC, EVDATA )
        CALL VZERO( Y, LFADC )
        IP = 1
   29   LON = EVDATA(IP)
        IF( LON .NE. 0) THEN
          IFIR = EVDATA(IP+1)
          IP   = IP + 2
          DO 30 K = 1,LON
            Y(IFIR+K) = UPSID * FLOAT(EVDATA(IP))
            IP = IP + 1
   30     CONTINUE
          GOTO 29
        ENDIF
        CALL PUVPRT(VXMIN,VXMAX,VYMIN,VYMAX)
        IF ( IWIR.GT.2 .AND. IWIR.LE.(NBSENS+2) ) THEN
          COLOR = 'GRE'
        ELSE
          COLOR = 'MAG'
        ENDIF
C
        CALL PUHIST( LFADC, Y, COLOR )
C
   20 CONTINUE
      RETURN
      END
