      SUBROUTINE PVTSEC_GEO(SECMIN, SECMAX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Displays the chamber between the sectors SECMIN and
C-   SECMAX ( the # are given or the layers 2 and 3 which contain 32 sectors).
C-
C-   Inputs  :
C-   Outputs :
C-
C-   Created  27-JUL-1990   Qizhong Li-Demarteau  from PVTSEC to PVTSEC_GEO
C-   Updated  16-MAR-1993   Alexandre Zinchenko - remove call to ZGVSEC
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GRAPHF77.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER NBSENS
      PARAMETER ( NBSENS = 8 )
C
C  Local Declarations:
C  ===================
C
      INTEGER SECMIN, SECMAX, SECMX, IS
      INTEGER I,L
      INTEGER NHITS(0:NBSENS-1),NWIR,LHIT
      INTEGER KPWIRE(0:NBSENS-1),KP
      INTEGER LAY, SEC, IWIR, ISEG
      INTEGER IFISAJ, IFDSEC, IFDWIR, IFDHIT, IFDSEG
      REAL    XWIR, YWIR, XCENT, YCENT, XORI, YORI
      INTEGER NBWIR, NSEC
      REAL    PHIW, CPHIW, SPHIW
      REAL    DDIS1,DDIS, YPOS
      REAL    XHPOS,YHPOS
      REAL    SIZDIS, DEGRAD
      REAL    RAYCEN, DELRAY, PHICEN, DELPHI
      REAL    X1, X2, Y1, Y2, R1, R2, PHI1, PHI2
      INTEGER LVRFT, LVALS, IPWIR,IER,ITYP
      PARAMETER( DEGRAD = 3.1415926535/180.)
      CHARACTER*4 REM,CVAL
      LOGICAL EZERROR
C
C  Data Statements:
C  ================
C
      DATA SIZDIS / 0.05 /
C----------------------------------------------------------------------
C
C ****  Pick correct RCP bank
C
      CALL EZPICK('PX_CALDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PVTSEC_GEO',
     &       'Cannot find PX_CALDIS_RCP','W')
        GOTO 999
      ENDIF
C
C ****  Get some CALDIS constants
C
      CALL EZ_GET_ARRAY('CALDIS_PXPARAMS','ISAJET TRACKS',1,IFISAJ,
     &     CVAL,ITYP,REM,IER)
      CALL EZ_GET_ARRAY('CALDIS_PXPARAMS','VTX DRAW SECTOR',1,IFDSEC,
     &     CVAL,ITYP,REM,IER)
      CALL EZ_GET_ARRAY('CALDIS_PXPARAMS','VXT DRAW WIRES',1,IFDWIR,
     &     CVAL,ITYP,REM,IER)
C
C ****  Draw Isajet tracks
C
      IF( IFISAJ .NE. 0 )CALL PVISAJ
C
      DO 88 LAY = 0,2
C
C ****  Access VRFT bank for nominal geometry
C
        LVRFT = LC( LVGEH - 3 )
        NSEC  = IC( LVRFT+ 2+ 7*LAY )
        SECMX = SECMAX
        IF ( SECMAX .GT. NSEC-1 ) SECMX = NSEC-1
        RAYCEN = C( LVRFT+ 7+ 7*LAY )
        PHICEN = C( LVRFT+ 8+ 7*LAY ) * DEGRAD
        DELRAY = C( LVRFT+ 5+ 7*LAY )
        DELPHI = C( LVRFT+ 6+ 7*LAY ) * DEGRAD
        R1 = ( RAYCEN - DELRAY ) / COS( DELPHI )
        R2 = ( RAYCEN + DELRAY ) / COS( DELPHI )
        DO 89 IS = SECMIN, SECMX
          SEC = IS
          IF( SEC .LT. 0  ) SEC = SEC + NSEC
          IF( SEC .GT. NSEC-1 ) SEC = SEC - NSEC
          CALL PUOPEN
          CALL PXCOLR('GRE')
          IF ( IFDSEC .NE. 0 ) THEN
C
C ****  Draw the cell limits.
C
            PHI1 = PHICEN + (2*SEC-1) * DELPHI
            PHI2 = PHICEN + (2*SEC+1) * DELPHI
            X1 =  R1 * COS( PHI1 )
            Y1 =  R1 * SIN( PHI1 )
            CALL JMOVE( X1, Y1 )
            X2 =  R1 * COS( PHI2 )
            Y2 =  R1 * SIN( PHI2 )
            CALL JDRAW( X2, Y2 )
            X2 =  R2 * COS( PHI2 )
            Y2 =  R2 * SIN( PHI2 )
            IF ( IFDSEC .GE. 2 ) THEN   ! draw full cells
              CALL JDRAW( X2, Y2 )
            ELSE                        ! don't draw inter-sector boundaries
              CALL JMOVE( X2, Y2 )
            ENDIF
            X2 =  R2 * COS( PHI1 )
            Y2 =  R2 * SIN( PHI1 )
            CALL JDRAW( X2, Y2 )
            IF ( IFDSEC .GE. 2 )  CALL JDRAW( X1, Y1 )
          ENDIF
          CALL PXCOLR( 'MAG' )
C          CALL ZGVSEC( LAY, SEC, NBWIR, KPWIRE(0), NHITS(0), LHIT)
          LVALS = LC( LC( LC( LSVTX-5 ) -(LAY+1) ) -(SEC+1) )
          CPHIW = C( LVALS+3 )
          SPHIW = C( LVALS+4 )
          DO 99 IWIR= 0, 7 !NBWIR-1
            IPWIR = LVALS + 6 + IC(LVALS+6) * IWIR
            XWIR = C( IPWIR+1 )
            YWIR = C( IPWIR+2 )
C
C  Mark wire positions with an "+"
C  ================================
C
            IF ( IFDWIR .NE. 0 ) THEN
              CALL JCMARK(1)
              CALL JMARK( XWIR, YWIR )
            ENDIF
   99     CONTINUE
          CALL JRCLOS
   89   CONTINUE
   88 CONTINUE
C

C----------------------------------------------------------------------
  990 CONTINUE
C
C ****  Reset RCP bank
C
      CALL EZRSET
  999 RETURN
      END
