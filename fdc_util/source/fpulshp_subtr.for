      SUBROUTINE FPULSHP_SUBTR(EXPDAT,NPULSE,UNIT,WIRE,IFIRST,IPEAK,
     &     ITAIL,IPEV,LENCLU,AREA,LEADING_EDGE,PEDLVL,RISETIME,
     &     PUL_RETRY,TEXT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Perform pulse shape subtraction.
C-
C-   Inputs  : EXPDAT = FADC channel data
C-             NPULSE = number of pulses on wire channel
C-             UNIT   = FDC Unit number
C-             WIRE   = FDC Wire number
C-             IFIRST = first bin of found pulse
C-             IPEAK  = peak bin of found pulse
C-             ITAIL  = tail bin of found pulse
C-             IPEV   = cluster location in FADC channel data
C-             LENCLU = length of cluster
C-             AREA   = area under pulse (less pedestal)
C-             LEADING_EDGE = leading edge as found by hitfinding
C-             PEDLVL = Pedestal level of channel
C-             TEXT   = plot text label
C-   Outputs : EXPDAT = FADC channel data
C-             PUL_RETRY = set TRUE if pulse has been subtracted from
C-                         FADC channel data
C-
C-   Created  14-JAN-1991   Jeffrey Bantly
C-   Updated  26-APR-1991   Jeffrey Bantly  cleanup PARAMS,RCP 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INCLUDE 'D0$INC:FDEVNT.INC'
C
      INTEGER IBIN,ICALL,IER,I
      INTEGER IPULWID,IFIRST,IPEAK,ITAIL,IPEV
      INTEGER CUTOFF,NPULSE,UNIT,WIRE
      INTEGER EXPDAT(0:LFADC-1),LENCLU,IADD
      INTEGER RUNTYPE
C
      REAL    FADC,FACTOR,FACTOR2,PEAK,AREA
      REAL    FBIN,NEW_FADC,PEDLVL,LEADING_EDGE,PKHGT
      REAL    PULFAC
      REAL    RISETIME,PAR2
C
      DOUBLE PRECISION FVGAUSS_TAIL
C
      CHARACTER*44 TEXT
C
      LOGICAL PUL_RETRY
C
      SAVE ICALL
      DATA ICALL/0/
C----------------------------------------------------------------------
C
      PUL_RETRY=.FALSE.
C
      IF (ICALL.EQ.0) THEN
        ICALL = 1
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET_i('RUNTYPE',RUNTYPE,IER)
        CALL EZGET('PULFAC',PULFAC,IER)
        CALL EZRSET
        IF(RUNTYPE.GT.0) THEN
          CUTOFF=210
        ELSE
          CUTOFF=130
        ENDIF
      ENDIF
C
      IPULWID=INT( HITS(4,NPULSE,WIRE)/NBPBIN )
      PKHGT=FLOAT(EXPDAT(IPEV+IPEAK))-PEDLVL+
     &   (ABS(FLOAT(EXPDAT(IPEV+IPEAK-1)-EXPDAT(IPEV+IPEAK+1)))/2.)
      FACTOR=1./(FLOAT(EXPDAT(IPEV+IPEAK))-PEDLVL)
      FACTOR2=1./PKHGT
C
      IADD=0
      IF(UNIT.EQ.1) IADD=IADD+12
      IF(WIRE.EQ.0) IADD=IADD+4
      IF(WIRE.GT.7 .AND. UNIT.EQ.0) IADD=IADD+8
      IF(WIRE.EQ.15 .AND. UNIT.EQ.1) IADD=IADD+8
C
C ****  Do pulse shape subtraction if necessary.
C
      FADC=FLOAT(EXPDAT(IPEAK+IPEV))-PEDLVL
      FADC=FADC*PULFAC
      PAR2=RISETIME*2.0
      DO 300 IBIN=IFIRST+IPEV,ITAIL+IPEV
        FBIN=FLOAT(IBIN-IPEV)-LEADING_EDGE
        NEW_FADC=FVGAUSS_TAIL(FBIN,PAR2)*FADC
        EXPDAT(IBIN)=EXPDAT(IBIN)-NEW_FADC
        IF(EXPDAT(IBIN).LT.0.) EXPDAT(IBIN)=0.0
  300 CONTINUE
C
cm      IF(HITS(4,NPULSE,WIRE).GE.CUTOFF .AND. AREA.GT.800.) THEN
        PUL_RETRY=.TRUE.
cm      ENDIF
C
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
