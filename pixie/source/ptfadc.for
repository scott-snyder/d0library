      SUBROUTINE PTFADC(WIRE,LAYER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Prints FADC histograms for the TRD wires
C-
C-   Inputs  : WIRE number and LAYER (if LAYER=0 plots for three layers)
C-   Outputs :
C-   Controls:
C-
C-   Created   1-JUN-1990   Norman A. Graf
C-   Updated   6-JUN-1990   Norman A. Graf  now calls PUHIST
C-   Updated  27-NOV-1990   Lupe Howell  Implementing PIXIE_RCP
C-   Updated  15-APR-1991   Lupe Howell  Hardcopy option only if TRD ONLY
C-   Updated  17-SEP-1991   JFG Use of the TRD_ANO_CATH flag
C-   Updated  15-FEB-1994   A. Zylberstejn: Protect against the absence of CDD4
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GRAPHF77.INC'
      INCLUDE 'D0$INC:WORKSP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER I,J,ILO,IHI,ICH2
      INTEGER WIRE,LAYER,lout,trunit
      INTEGER CHA1,ICH,UBIT,TDATA(260)
      INCLUDE 'D0$LINKS:IZCDD4.LINK'
      logical doprint,TRD_DO_PRINT,FIRST
      REAL WG,PEDES,VSUM
      INTEGER IERR,NTFADC
      INTEGER KA
      LOGICAL TRUTH,FLGVAL
C
      REAL  VYMIN, VYMAX, VXMIN, VXMAX
      REAL  Y(1000)
      CHARACTER*3 CWIR    ! LABEL VARIABLES
      CHARACTER*2 CLAY
      CHARACTER*4 CVAL, REM
      INTEGER TYP,IER
      INTEGER FADC_START,FADC_STOP
      INTEGER LFADC
      REAL DISP,SPACE

      LOGICAL EZERROR
      LOGICAL DRAW,ALL_LAYERS,SUB_PEDES,DO_GAIN
C--------------------------------------------------------------------
      DATA DISP/.50/
      DATA SPACE/.14/
      data first/.true./
C----------------------------------------------------------------------
C
      if(first)then
        first=.false.
        lout=trunit()
      end if
      doprint=TRD_DO_PRINT()
      DRAW = .TRUE.
C
C ****  Picking PIXIE RCP
C
      CALL EZPICK('PX_TRDDIS_RCP')
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PTFADC','Cannot find PX_TRDDIS_RCP','W')
        GOTO 999
      ENDIF
C
C ****  Get some TRD constants
C
      CALL EZ_GET_ARRAY('PXPARAMS','TRD ALL LAY',1,ALL_LAYERS,
     &       CVAL,TYP,REM,IER)
      IF ( IER .NE. 0 ) THEN
        CALL ERRMSG('PIXIE','PTFADC',
     &      'PXPARAMS NOT FOUND','W')
        GOTO 900
      ENDIF
      CALL EZ_GET_ARRAY('PXPARAMS','TRD PEDESTALS',1,SUB_PEDES,
     &       CVAL,TYP,REM,IER)
      CALL EZ_GET_ARRAY('PXPARAMS','TRD GAINS',1,DO_GAIN,
     &       CVAL,TYP,REM,IER)
      CALL EZGET('FADC_START',FADC_START,IER)
      CALL EZGET('FADC_STOP',FADC_STOP,IER)
C      CALL PUHEAD('TRD FADC')    ! Clears screen
C
C ****  Get the FADC data
C
      TRUTH = FLGVAL('TRD_ANO_CATH')
      IF (TRUTH.EQ..TRUE.) THEN
        KA = 0
      ELSE IF (TRUTH.EQ..FALSE.) THEN
        KA = 3
      ENDIF
      ILO = LAYER
      IHI = LAYER
      IF(ALL_LAYERS) THEN
        ILO = 1 + KA
        IHI = 3 + KA
      ENDIF
          IF(LQ(LHEAD-IZCDD4).eq.0)THEN
            CALL ERRMSG('ERROR reading CDD4 ','PTFADC',' ','W')
            go to 999
            end if
      DO ICH = ILO,IHI
        CALL TCODER(CHA1,ICH-1,WIRE-1,UBIT,2)
        CALL ZDEXPD(4,CHA1,TDATA)
        IF(TDATA(1).GT.0.AND.FADC_START.LE.TDATA(1)) THEN
          FADC_STOP = MIN(FADC_STOP,TDATA(1))
          NTFADC = FADC_STOP - FADC_START + 1
          CALL VFLOAT(TDATA(3),WS,TDATA(1))
          IF( SUB_PEDES ) THEN
C  compute ped from 10 first channels
            PEDES=VSUM(WS,10)*.1
C            CALL TRGPED('BID',WIRE,ICH,PEDES,IERR)
C
C ****  Subtract pedestals
C
            CALL VBIAS(WS,-PEDES,WS,TDATA(1))
          ENDIF
C
C
          IF (TRUTH) THEN
            ICH2 = ICH
          ELSE
            ICH2 = ICH - 3
          ENDIF
C
C ****  Correct for wire gain
C
c          IF( DO_GAIN ) THEN
c            CALL TRGGN('BID',WIRE,ICH2,WG,IERR)
C This will have to be changed (JFG 5/16/91: anodes and cathodes have
C                               same gain).
c            IF (WG.GT.0.) THEN
c              WG = 1./WG
c            ELSE
c              WG = 0.
c            ENDIF
c            CALL VSCALE(WS,WG,WS,TDATA(1))
c          ENDIF
C
C ****  Now plot it
C
          WRITE(CLAY,1000)ICH
 1000     FORMAT(I2)
          WRITE(CWIR,1001)WIRE
 1001     FORMAT(I3)
C
C ****   SETTING WINDOW
C
          VXMAX = 1.
          VXMIN = -1.
          VYMAX = 0.78 - (SPACE * (ICH2-1)) - (DISP * (ICH2-1)) + 0.1
          VYMIN = VYMAX - 0.1
          CALL PUVPRT(VXMIN,VXMAX,VYMIN,VYMAX)
          CALL JWINDO(-50.,50.,-50.,50.)
          CALL PUOPEN
          CALL PXCOLR('BLU')
          CALL JJUST(2,2)
          CALL JSIZE(1.4560,25.5660)
          CALL JMOVE(0.,40.)
          CALL J1STRG('LAYER   '//CLAY )
          CALL JMOVE(0.,0.)
          CALL J1STRG('WIRE    '//CWIR )
          CALL JRCLOS
          CALL VZERO(  Y, NTFADC)
          DO 10 J=1,NTFADC
            IF(SUB_PEDES .OR. DO_GAIN) THEN
              Y(J) = WS(J+FADC_START-1)
            ELSE
              Y(J) = TDATA(J+FADC_START+1)
            ENDIF
   10     CONTINUE
          VYMAX = 0.78 - (SPACE * (ICH2-1)) - (DISP * (ICH2-1))
          VYMIN = VYMAX - (DISP - 0.01)
          CALL PUVPRT(VXMIN,VXMAX,VYMIN,VYMAX)
C          CALL PXHIST( LFADC, Y, 'RED' )
          CALL PUHIST( NTFADC, Y, 'RED' )
        ENDIF
      ENDDO
      CALL PURSET
  900 CONTINUE
C
C ****  Reseting RCP file
C
      CALL EZRSET
  999 continue
      RETURN
      END
