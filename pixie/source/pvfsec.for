      SUBROUTINE PVFSEC ( DETTYP )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Plot FADC traces for the 16 channels in a VTX
C-                         sector (DETTYP = 0) or 16 consecutive strip
C-                         channels (DETTYP = 1).
C-
C-   Inputs  : DETTYP [I] = 0 for wire display, = 1 for strip display
C-   Outputs : the display
C-
C-   Created  18-OCT-1989   Peter Grudberg
C-   Updated  24-SEP-1990   Lupe Howell  Implementing PIXIE_RCP
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PXPARA.INC'

      INTEGER LFADC, NBCHAN
      PARAMETER ( LFADC = 512 )
      PARAMETER ( NBCHAN = 16 )
      INTEGER DETTYP, NBSTRP, NBEND, ISTRIP, ISTR
      INTEGER LAYER, SECTOR, STRIP, END, IP, BIN
      INTEGER IWIR, IEND, ADRESS, UBIT, IFIRST
      CHARACTER*3 COLOR
      INTEGER EVDATA(LFADC), ZEND, LONG
      INTEGER MAXSTR(0:5)
      REAL XPOS, YPOS, VXMIN, VXMAX, VYMIN, VYMAX
      REAL YWID, YSEP, YSIZ, UPSIDN, Y(LFADC)
      CHARACTER*40 TITLE
      CHARACTER*9 LABEL
      CHARACTER*6 LABELS(0:7)
      CHARACTER*4 CVAL, REM
      INTEGER IER,TYP
      LOGICAL EZERROR
      DATA MAXSTR / 0, 0, 160, 192, 192, 128 /
      DATA LABELS / 'Wire 0', 'Wire 1', 'Wire 2', 'Wire 3',
     &              'Wire 4', 'Wire 5', 'Wire 6', 'Wire 7' /
C----------------------------------------------------------------------
C
C ****  Get channels to plot:
C
      IF ( DETTYP .EQ. 0 ) THEN
C
C ****  Picking PIXIE RCP
C
        CALL EZPICK('PX_VTXDIS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('PIXIE','PVFSEC','Cannot find PX_VTXDIS_RCP','W')
          GOTO 999
        ENDIF
C
C ****  Get some VTX constants
C
        CALL EZ_GET_ARRAY('PXPARAMS','VTX LAYER',1,LAYER,CVAL,
     &       TYP,REM,IER)
        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG('PIXIE','PVFSEC',
     &      'PXPARAMS NOT FOUND','W')
          GOTO 900
        ENDIF
        CALL EZ_GET_ARRAY('PXPARAMS','VTX SECTOR',1,SECTOR,CVAL,
     &       TYP,REM,IER)
      ELSE
        CALL EZ_GET_ARRAY('PXPARAMS','VTX STRIP LAYER',1,LAYER,
     &       CVAL,TYP,REM,IER)
        CALL EZ_GET_ARRAY('PXPARAMS','VTX STRIP',1,STRIP,CVAL,
     &       TYP,REM,IER)       ! Starting strip #
        IF ( LAYER .EQ. 2 ) THEN
          CALL EZ_GET_ARRAY('PXPARAMS','VTX STRIP END',1,END,
     &       CVAL,TYP,REM,IER)       ! Starting strip #
C
C ****  Strips on layer 2 are split at z=0 and read out at both ends.
C ****  For strip layers 3-5, some strips are read out at end 0, and some are
C ****  read out at end 1 (not both).  The distribution of channels to the
C ****  two ends is not uniform between layers, and is not easily specified.
C ****  This routine looks for data from both ends of each strip.  Only the
C ****  instrumented end can contain data, so there shouldn't be a problem.
C
        ENDIF
      ENDIF
C
C ****  Plot title
C
      CALL PUOPEN
      IF ( DETTYP .EQ. 0 ) THEN
        WRITE (TITLE,100) LAYER, SECTOR
  100   FORMAT('VTX Wire Layer ',I1,' Sector ',I2)
      ELSE
        WRITE (TITLE,110) LAYER, STRIP
  110   FORMAT('VTX Strip Layer ',I1,' Starting Strip ',I3)
      ENDIF
      CALL PXCOLR('FOR')
      CALL JJUST(2,2)
      XPOS = 0.
      YPOS = YWIND2 * 0.95
      CALL PUVSTR( XPOS, YPOS, 1.5, 2.0, TITLE )
      CALL JRCLOS
C
      VXMIN = -0.8
      VXMAX =  1.0
      VYMAX =  0.9
      YWID  =  1.85 / NBCHAN
      IF (DETTYP .EQ. 0 .OR. (DETTYP .EQ. 1 .AND. LAYER .EQ. 2)) THEN
        YSEP  =  0.05 * YWID
        VYMIN =  VYMAX + YSEP
      ELSE
        YSEP  =  0.1 * YWID
        VYMIN =  VYMAX
      ENDIF
      YSIZ  =  YWID - YSEP
C
C ****  do the plot:
C
      IF ( DETTYP .EQ. 0 ) THEN
C
C ****  Wire plots: plot +z end up, -z end down, share x axis
C
        DO IWIR = 0, 7
          CALL JWINDO(-1.,1.,-1.,1.)
          CALL PUVPRT(-1.,1.,-1.,1.)
          CALL PUOPEN
          CALL PXCOLR('FOR')
          CALL JJUST(2,2)
          XPOS = -0.93
          YPOS = VYMIN - YWID - 2 * YSEP
          CALL PUVSTR( XPOS, YPOS, 1.0, 2.0, LABELS(IWIR) )
          CALL JRCLOS
          IF ( IWIR .EQ. 0 ) THEN
            CALL PUOPEN
            CALL JJUST(2,2)
            XPOS = -0.81
            YPOS = YPOS + YWID / 2.
            CALL PUVSTR( XPOS, YPOS, 1.0, 2.0, '+Z' )
            YPOS = YPOS - YWID
            CALL PUVSTR( XPOS, YPOS, 1.0, 2.0, '-Z' )
            CALL JRCLOS
          ENDIF
          DO IEND = 1, 0, -1            ! +z end (end 1) first
            IF ( IEND .EQ. 0 ) THEN
              UPSIDN = -1.0
              VYMAX  = VYMIN
              COLOR  = 'FOR'
            ELSE
              UPSIDN = 1.0
              VYMAX  = VYMIN - 2. * YSEP
              COLOR  = 'FOR'
            ENDIF
            VYMIN = VYMAX - YSIZ
C
C ****  Fill Y with FADC data:
C
            CALL VZERO( Y, LFADC )
            CALL VCODER( ADRESS, DETTYP, LAYER, SECTOR, IWIR,
     &                   STRIP, IEND, UBIT, 2 )
            CALL VTUNPK( ADRESS, EVDATA )
            IP = 1
   29       LONG = EVDATA(IP)
            IF ( LONG .GT. 0 ) THEN
              IFIRST = EVDATA(IP+1)
              IP = IP + 2
              DO BIN = 0, LONG - 1
                Y(IFIRST+BIN) = UPSIDN * FLOAT(EVDATA(IP))
                IP = IP + 1
              ENDDO
              GOTO 29
            ENDIF
C
C ****  If the channel was empty, set Y(1)=UPSIDN to make the plot correct
C
            IF ( IP .EQ. 1 ) Y(1) = UPSIDN
            CALL PUVPRT(VXMIN,VXMAX,VYMIN,VYMAX)
C
            CALL PUHIST( LFADC, Y, COLOR )
C
          ENDDO                         ! end
        ENDDO                           ! wire
      ELSE
C
C ****  For strips, all the plots are up except for when LAYER = 2 and both
C ****  ends are to be plotted (END=2).
C
        IF ( LAYER .EQ. 2 .AND. END .EQ. 2 ) THEN
          NBSTRP = NBCHAN / 2
          NBEND  = 2
        ELSE
          NBSTRP = NBCHAN
          NBEND = 1
        ENDIF
C
        DO ISTRIP = STRIP, STRIP + NBSTRP - 1
          ISTR = MOD(ISTRIP,MAXSTR(LAYER))      ! allow wraparound
          CALL PUVPRT(-1.,1.,-1.,1.)
          CALL JWINDO(-1.,1.,-1.,1.)
          CALL PUOPEN
          CALL PXCOLR('FOR')
          CALL JJUST(2,2)
          XPOS = -0.9
          IF ( NBEND .EQ. 2 ) THEN
            YPOS = VYMIN - YWID - 2. * YSEP
          ELSE
            YPOS = VYMIN - YWID / 2.
          ENDIF
          WRITE (LABEL,120) ISTR
  120     FORMAT('Strip ',I3)
          CALL PUVSTR( XPOS, YPOS, 1.0, 2.0, LABEL )
          CALL JRCLOS
          IF ( NBEND .EQ. 2 .AND. ISTR .EQ. STRIP ) THEN
            CALL PUOPEN
            CALL JJUST(2,2)
            XPOS = -0.81
            YPOS = YPOS + YWID / 2.
            CALL PUVSTR( XPOS, YPOS, 1.0, 2.0, '+Z' )
            YPOS = YPOS - YWID
            CALL PUVSTR( XPOS, YPOS, 1.0, 2.0, '-Z' )
            CALL JRCLOS
          ENDIF
          DO IEND = NBEND - 1, 0, -1    ! +z end (end 1) first
            IF ( NBEND .EQ. 2 ) THEN
              IF ( IEND .EQ. 0 ) THEN
                UPSIDN = -1.0
                VYMAX = VYMIN
                COLOR = 'FOR'
              ELSE
                UPSIDN = 1.0
                VYMAX  = VYMIN - 2. * YSEP
                COLOR  = 'FOR'
              ENDIF
            ELSE
              UPSIDN = 1.0
              VYMAX = VYMIN - YSEP
            ENDIF
            VYMIN = VYMAX - YSIZ
C
C ****  Fill Y with FADC data:
C
            CALL VZERO( Y, LFADC )
            IF ( LAYER .EQ. 2 .AND. END .NE. 2 ) THEN
              ZEND = END
            ELSE
              ZEND = IEND
            ENDIF
            CALL VCODER( ADRESS, DETTYP, LAYER, SECTOR, IWIR,
     &                   ISTR, ZEND, UBIT, 2 )
   39       CALL VTUNPK( ADRESS, EVDATA )
            IP = 1
   49       LONG = EVDATA(IP)
            IF ( LAYER .GT. 2 .AND.
     &           (IP .EQ. 1 .AND. LONG .EQ. 0) ) THEN
C
C ****  Look for data on the other end (only one read out)
C
              ZEND = MOD( ADRESS, 2 )
              IF ( ZEND .EQ. 0 ) THEN
                ADRESS = ADRESS + 1
                GOTO 39
              ENDIF
            ENDIF
            IF ( LONG .GT. 0 ) THEN
              IFIRST = EVDATA(IP+1)
              IP = IP + 2
              DO BIN = 0, LONG - 1
                Y(IFIRST+BIN) = UPSIDN * FLOAT(EVDATA(IP))
                IP = IP + 1
              ENDDO
              GOTO 49
            ENDIF
            IF ( IP .EQ. 1 ) Y(1) = UPSIDN
            CALL PUVPRT(VXMIN,VXMAX,VYMIN,VYMAX)
C
            CALL PUHIST( LFADC, Y, COLOR )
C
          ENDDO
        ENDDO
      ENDIF
C
C ****  Plot a scale along the bottom
C
      CALL PUVPRT(VXMIN,VXMAX,-1.0,-0.96)
      COLOR = 'FOR'
      CALL PVSCAL( LFADC, 1, COLOR )
      CALL PUVPRT(-1.,1.,-1.,1.)
      CALL JWINDO(-1.,1.,-1.,1.)
      CALL PUOPEN
      CALL PXCOLR('FOR')
      CALL JJUST(2,2)
      XPOS = -0.86
      YPOS = -0.98
      CALL PUVSTR( XPOS, YPOS, 0.75, 2.0, '10 bins/div' )
      CALL JRCLOS
C
C ****  Reseting RCP file
C
  900 CALL EZRSET
  999 RETURN
      END
