      SUBROUTINE PVTSEC ( SECMIN, SECMAX )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Displays the chamber between the sectors SECMIN and
C-   SECMAX ( the # are given or the layers 2 and 3 which contain 32 sectors).
C-   also displays the event in this part of the chamber.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-
C-   Created  27-SEP-1988   Ghita Rahal-Callot : Adapted from the routine
C-                                               PVCOSE
C-   Updated  10-JAN-1990   Lupe Howell: Implementing Color Table
C-   Updated  17-JAN-1990   Peter Grudberg  Fix bug in wire staggering
C-   Updated  10-APR-1990   Peter Grudberg : make intersector boundaries
C-                                           optional
C-   Updated  24-SEP-1990   Lupe Howell  Implementing PIXIE_RCP
C-   Updated  30-JAN-1991   Lupe Howell  The number of segments opened
C-      decreased 
C-   Updated  07-AUG-1991   An-Dien Nguyen: track numbers in RPhi view
C-   Updated  16-MAR-1993   Alexandre Zinchenko - add call to PVXYHT_CMPRS
C-                                               for compressed hits
C    Updated  27-june-1994  Danilo Puseljic Added a call to VCHT_UNPACK to 
C                           enable display from VCHT banks
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
      INTEGER SECMIN, SECMAX, SECMX, IS, L
      INTEGER NHITS(0:NBSENS-1),LHIT
      INTEGER KPWIRE(0:NBSENS-1),KP
      INTEGER LAY, SEC, IWIR, DRWLBL
      INTEGER IFISAJ, IFDSEC, IFDWIR, IFDHIT, IFDSEG, IFDLBL
      INTEGER LVCHT,GZVCHT,ISTAT
      REAL    XWIR, YWIR
      INTEGER NBWIR, NSEC
      REAL    CPHIW, SPHIW
      REAL    DDIS, YPOS
      REAL    XHPOS,YHPOS
      REAL    SIZDIS, DEGRAD
      REAL    RAYCEN, DELRAY, PHICEN, DELPHI
      REAL    X1, X2, Y1, Y2, R1, R2, PHI1, PHI2
      INTEGER LVRFT, LVALS, IPWIR
      INTEGER MAXVHIT,NPLOT
      INTEGER IER, TYP, LVHIT, GZVHIT
      PARAMETER( DEGRAD = 3.1415926535/180.)
      CHARACTER*4 CVAL, REM
      CHARACTER*12 IMESS1,CMAXHIT
      CHARACTER*24 IMESS2
      LOGICAL EZERROR
C
C  Data Statements:
C  ================
C
      DATA SIZDIS / 0.05 /
      DATA MAXVHIT/0/
      DATA IMESS1/' ERR VHITMAX'/
C=====================================================================
C
C ****  Picking PIXIE RCP
C
      CALL EZPICK('PX_VTXDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PVTSEC','Cannot find PX_VTXDIS_RCP','W')
        GOTO 999
      ENDIF
C
C ****  Get some VTX constants
C
      CALL EZ_GET_ARRAY('PXPARAMS','ISAJET TRACKS',1,IFISAJ,CVAL,
     &       TYP,REM,IER)
      IF ( IER .NE. 0 ) THEN
        CALL ERRMSG('PIXIE','PVTSEC',
     &      'PXPARAMS NOT FOUND','W')
        GOTO 900
      ENDIF
      CALL EZ_GET_ARRAY('PXPARAMS','VTX DRAW SECTORS',1,IFDSEC,
     &       CVAL,TYP,REM,IER)
      CALL EZ_GET_ARRAY('PXPARAMS','VTX DRAW WIRES',1,IFDWIR,
     &       CVAL,TYP,REM,IER)
      CALL EZ_GET_ARRAY('PXPARAMS','VTX DRAW HITS',1,IFDHIT,
     &       CVAL,TYP,REM,IER)
      CALL EZ_GET_ARRAY('PXPARAMS','VTX MAX HITS',1,MAXVHIT,
     &       CVAL,TYP,REM,IER)
      IF(MAXVHIT.LE.0)MAXVHIT=9999
      CALL EZ_GET_ARRAY('PXPARAMS','VTX DRAW TRACKS',1,IFDSEG,
     &       CVAL,TYP,REM,IER)
      CALL EZ_GET_ARRAY('PXPARAMS','VTX DRAW LABEL',1,IFDLBL,
     &       CVAL,TYP,REM,IER)
      NPLOT=0
C
C ****  Draw Isajet tracks
C
      IF( IFISAJ .NE. 0 )CALL PVISAJ
C
      CALL PUOPEN
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
          CALL PXCOLR('BLU')
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
C
C  -- Get the hits either from VSEC,VHIT or VCHT banks
          IF (IFDHIT .NE. 0) THEN 
            CALL ZGVSEC( LAY, SEC, NBWIR, KPWIRE(0), NHITS(0), LHIT)
            IF (LHIT.EQ.0) THEN
              LVHIT = GZVHIT() 

              IF (LVHIT .NE. 0) THEN 
                CALL PVXYHT_CMPRS(LAY,SEC,NPLOT) 
                IF (NPLOT.GE.MAXVHIT) GO TO 950
              ELSE
                LVCHT = GZVCHT() 
                IF (LVCHT .NE. 0) THEN 
                  CALL VCHT_UNPACK(LAY,SEC,ISTAT)
                  CALL ZGVSEC(LAY,SEC,NBWIR,KPWIRE(0),NHITS(0),LHIT)
                ENDIF
              ENDIF

            ENDIF
          ENDIF
C
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
              NPLOT=NPLOT+1
              IF(NPLOT.GE.MAXVHIT)GO TO 950
            ENDIF
            IF ( LHIT .EQ. 0) GO TO 99
            IF ( IFDHIT .NE. 0 ) THEN
              DO 66 L = 1,NHITS(IWIR)

              NPLOT=NPLOT+1
              IF(NPLOT.GE.MAXVHIT)GO TO 950
C  Get drift distances from ZEBRA banks and mark raw hit positions
C  with a short segment
C  ================================================================

                KP = KPWIRE( IWIR ) + (L-1)*LHIT + 2
                YPOS = (-1.)**IS * C (LVRFT + 31 + IWIR)
                DDIS = Q(KP) - YPOS - .5*SIZDIS
                XHPOS = XWIR + DDIS * CPHIW
                YHPOS = YWIR + DDIS * SPHIW
                CALL JMOVE( XHPOS, YHPOS )
                XHPOS = XHPOS + SIZDIS * CPHIW
                YHPOS = YHPOS + SIZDIS * SPHIW
                CALL JDRAW( XHPOS, YHPOS )
                DDIS = Q(KP+1) - YPOS - .5*SIZDIS
                XHPOS = XWIR + DDIS * CPHIW
                YHPOS = YWIR + DDIS * SPHIW
                CALL JMOVE( XHPOS, YHPOS )
                XHPOS = XHPOS + SIZDIS * CPHIW
                YHPOS = YHPOS + SIZDIS * SPHIW
                CALL JDRAW( XHPOS, YHPOS )
   66         CONTINUE
            ENDIF
   99     CONTINUE
   89   CONTINUE
   88 CONTINUE
      CALL JRCLOS
C
C ****  Now, draw tracks
C
      DRWLBL = IFDLBL
      IF ( IFDSEG .NE. 0 ) CALL PVTRAK(DRWLBL)

C----------------------------------------------------------------------
C
C ****  Resetting RCP file
C
  900 CALL EZRSET
      GO TO 999
C Plotted max number of hits
  950 CALL JRCLOS
      CALL PXITOC(MAXVHIT,10,CMAXHIT)
      IMESS2=IMESS1//CMAXHIT
      CALL INTMSG(IMESS2)
      CALL EZRSET
  999 RETURN
      END
