      SUBROUTINE PV1ADC ( DETTYP )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Plot the FADC signal and first difference for a
C-                         single VTX channel, either wires (DETTYP=0) or
C-                         strips (DETTYP=1).
C-
C-   Inputs  : DETTYP [I] = 0 for wires, = 1 for strips
C-   Outputs : the display
C-
C-   Created  18-OCT-1989   Peter Grudberg
C-   Updated  24-SEP-1990   Lupe Rosas Howell  Implementing PIXIE_COMPACK
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PXPARA.INC'
      INTEGER LFADC
      PARAMETER ( LFADC = 512 )
      INTEGER DETTYP, EVDATA(LFADC), IP, ZEND
      INTEGER LAYER, SECTOR, WIRE, END, STRIP
      INTEGER LONG, IFIRST, BIN
      CHARACTER*3 COLOR
      REAL XPOS, YPOS
      REAL Y(LFADC), DY(LFADC)
      CHARACTER*40 TITLE
      INTEGER ADRESS, UBIT
      CHARACTER*4 CVAL, REM
      INTEGER IER,TYP
      LOGICAL EZERROR
      DATA UBIT / 0 /
C----------------------------------------------------------------------
C
C ****  Picking PIXIE RCP
C
      CALL EZPICK('PX_VTXDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PV1ADC','Cannot find PX_VTXDIS_RCP','W')
        GOTO 999
      ENDIF
C
C ****  Get channel to plot:
C
      IF ( DETTYP .EQ. 0 ) THEN
C
C ****  Get some VTX constants
C
        CALL EZ_GET_ARRAY('PXPARAMS','VTX LAYER',1,LAYER,CVAL,
     &       TYP,REM,IER)
        IF ( IER .NE. 0 ) THEN
          CALL ERRMSG('PIXIE','PV1ADC',
     &      'PXPARAMS NOT FOUND','W')
          GOTO 900
        ENDIF
        CALL EZ_GET_ARRAY('PXPARAMS','VTX SECTOR',1,SECTOR,CVAL,
     &       TYP,REM,IER)
        CALL EZ_GET_ARRAY('PXPARAMS','VTX WIRE',1,WIRE,CVAL,
     &       TYP,REM,IER)
        CALL EZ_GET_ARRAY('PXPARAMS','VTX WIRE END',1,END,CVAL,
     &       TYP,REM,IER)
C
C ****  encode ADRESS:
C
        CALL VCODER(ADRESS,DETTYP,LAYER,SECTOR,WIRE,STRIP,END,UBIT,2)
        COLOR = 'FOR'
      ELSE
        CALL EZ_GET_ARRAY('PXPARAMS','VTX STRIP LAYER',1,LAYER,
     &       CVAL,TYP,REM,IER)
        CALL EZ_GET_ARRAY('PXPARAMS','VTX STRIP',1,STRIP,CVAL,
     &       TYP,REM,IER)
        IF ( LAYER .EQ. 2 ) THEN        ! strips split at z=0
          CALL EZ_GET_ARRAY('PXPARAMS','VTX STRIP END',1,END,
     &       CVAL,TYP,REM,IER)
        ELSE
C
C ****  For strip layers 3-5, some strips are read out at end 0, and some are
C ****  read out at end 1.  The distribution of channels to the two ends is
C ****  not uniform between layers, and is not easily specified.  This routine
C ****  looks for data from both ends of each strip.  Only the instrumented end
C ****  can contain data, so there shouldn't be a problem.
C
          END = 0                       ! look at end 0 first
        ENDIF
        CALL VCODER(ADRESS,DETTYP,LAYER,SECTOR,WIRE,STRIP,END,UBIT,2)
        COLOR = 'FOR'
      ENDIF
C
C ****  Plot title
C
      CALL PUOPEN
      IF ( DETTYP .EQ. 0 ) THEN
        WRITE (TITLE,100) LAYER, SECTOR, WIRE, END
  100   FORMAT('VTX Wire Layer ',I1,' Sector ',I2,
     &         ' Wire ',I1,' End ',I1)
      ELSE
        IF ( LAYER .EQ. 2 ) THEN
          WRITE (TITLE,110) LAYER, STRIP, END
  110     FORMAT('VTX Strip Layer ',I1,' Strip ',I3,' End ',I1)
        ELSE
          WRITE (TITLE,120) LAYER, STRIP
  120     FORMAT('VTX Strip Layer ',I1,' Strip ',I3)
        ENDIF
      ENDIF
      CALL PXCOLR('FOR')
      CALL JJUST(2,2)
      XPOS = 0.
      YPOS = YWIND2 * 0.95
      CALL PUVSTR( XPOS, YPOS, 1.5, 2.0, TITLE )
      CALL JRCLOS
C
C ****  Fill Y with FADC data and DY with first difference:
C
      CALL VZERO( Y, LFADC )
      CALL VZERO( DY, LFADC )
   29 CALL VTUNPK( ADRESS, EVDATA )
      IP = 1
   39 LONG = EVDATA(IP)
      IF ( DETTYP .EQ. 1 ) THEN         ! deal with strip-end
        IF ( LAYER .GT. 2 .AND.
     &       (IP .EQ. 1 .AND. LONG .EQ. 0) ) THEN
C
C ****  Channel empty, look for data on the other end
C
          ZEND = MOD( ADRESS, 2 )
          IF ( ZEND .EQ. 0 ) THEN
            ADRESS = ADRESS + 1
            GOTO 29
          ENDIF
        ENDIF
      ENDIF
      IF ( LONG .GT. 0 ) THEN
        IFIRST = EVDATA(IP+1)
        IP = IP + 2
        DO BIN = 0, LONG - 1
          Y(IFIRST+BIN) = FLOAT( EVDATA(IP) )
          IP = IP + 1
          IF ( BIN .GT. 0 ) THEN
            DY(IFIRST+BIN-1) = Y(IFIRST+BIN) - Y(IFIRST+BIN-1)
          ENDIF
        ENDDO
        GOTO 39
      ENDIF
C
C ****  do the plot:
C
      CALL PUVPRT(-1.,1.,-1.,1.)
      CALL JWINDO(-1.,1.,-1.,1.)
      CALL PUOPEN
      CALL JJUST(2,2)
      CALL PXCOLR('FOR')
      CALL PUVSTR( -0.9, 0.45, 1.0, 2.0, 'FADC data')
      CALL JRCLOS
C
      CALL PUVPRT( -0.8, 1.0, 0.2, 0.7 )
      CALL PUHIST( LFADC, Y, COLOR )
C
      CALL PUVPRT(-1.,1.,-1.,1.)
      CALL JWINDO(-1.,1.,-1.,1.)
      CALL PUOPEN
      CALL JJUST(2,2)
      CALL PXCOLR('FOR')
      CALL PUVSTR( -0.9, -0.45, 1.0, 2.0, '1st Diff.')
      CALL JRCLOS
C
      CALL PUVPRT( -0.8, 1.0, -0.7, -0.2 )
      CALL PUHIST( LFADC-1, DY, COLOR )
C
C ****  Plot scale along center:
C
      CALL PUVPRT( -0.8, 1.0, 0.0, 0.06 )
      COLOR = 'FOR'
      CALL PVSCAL( LFADC, 1, COLOR )
      CALL PUVPRT( -0.8, 1.0, -0.06, 0.0 )
      COLOR = 'FOR'
      CALL PVSCAL( LFADC, -1, COLOR )
C
      CALL PUVPRT( -1.0, 1.0, -1.0, 1.0 )
      CALL JWINDO( -1.0, 1.0, -1.0, 1.0 )
      CALL PUOPEN
      CALL PXCOLR('FOR')
      CALL JJUST(2,2)
      CALL PUVSTR( -0.88, 0.0, 0.75, 2.0, '10 bins/div')
      CALL JRCLOS
C
C ****  Reseting RCP file
C
  900 CALL EZRSET
  999 RETURN
      END
