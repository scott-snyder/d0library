
      SUBROUTINE PLMARK_TRGR(IMARK,TXMIN,TXMAX,TYMIN,TYMAX,ZDIV)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Put marks on TRGR_LEGO PLOT
C-
C-   Inputs  : IMARK - Kind of mark to make .GT.0 Missing ET
C-             TXMIN - Min of y lego plot
C-             TXMAX - Max of x lego plot
C-             TYMIN - Min of y lego plot
C-             TYMAX - Max of y lego plot
C-             ZDIV  - Scale factor
C-
C-   Created   15-MAR-1993   Nobuaki Oshima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C ARGUMENTS:
      INTEGER IMARK    ! FLAG to tell which kind of mark to put
                       ! IMARK.GT.0 for Missing Et
      REAL TXMIN,TXMAX ! Min and max of lego plot x axis
      REAL TYMIN,TYMAX ! Min and max of lego plot y axis
      REAL ZDIV !Z SCALE FACTOR
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PI.DEF'
C------------------------------------------------------------------------
      INTEGER LTRGR_LEVEL1,GZTRGR,GZFIND_CRATE
      INTEGER IPHI,IETA
      REAL    ETA,ET,ETERR
      REAL    TX,TY,Z1,Z2,Z3,DELZ
      REAL    XSIZ,YSIZ
      REAL    TRGR_PX, TRGR_PY, TRGR_MPT, TRGR_MPT_PHI
      CHARACTER*3 COL1
      CHARACTER*8 TITLE
      DATA TITLE/'Miss ET '/
C------------------------------------------------------------------------
      IF (IMARK .LE. 0)   GO TO 999
C-
C--- Missing ET
C-
      LTRGR_LEVEL1 = GZFIND_CRATE ( 'TRGR', GZTRGR(), 11 )
      IF (LTRGR_LEVEL1 .LE. 0)   GO TO 999
      CALL L1EXTRACT_MOMENTUM ( IQ(LTRGR_LEVEL1),
     &                          TRGR_PX, TRGR_PY, TRGR_MPT )
C--- Recompute Mpt because its readout in TRGR isn't yet implemented 
      TRGR_MPT = SQRT( TRGR_PX**2 + TRGR_PY**2 )
      IF (TRGR_PX.NE.0.0 .AND. TRGR_PY.NE.0.0) THEN
C--- Invert sign to represent Missing Energy
        TRGR_MPT_PHI = ATAN2( -TRGR_PY, -TRGR_PX )  
      ELSE
        TRGR_MPT_PHI = 0.0
      ENDIF
C--- Convert [-pi:+pi] to [0:2pi]
      IF ( TRGR_MPT_PHI .LT. 0.0 ) 
     &  TRGR_MPT_PHI = TRGR_MPT_PHI + SNGL(TWOPI)
      IPHI = (TRGR_MPT_PHI/TWOPI)*32 + 1
      ET   = TRGR_MPT
      TX   = IPHI
C-
C--- Set missing Et at ETA=0.
      TY   = 20.
      IF (TX.LT.TXMIN .OR. TX.GT.TXMAX)   GO TO 100
      Z2   = ET/ZDIV
      CALL JLSTYL(1)
      CALL JPINTR(0) ! No fill
      CALL PXCOLR('MAG') ! Filling with magenta
C-
C--- DRAW VBAR
      CALL PLDBAR(3,TX,TX+1.,TY+.5,TY+1.5,0.,Z2,0)
C-
C--- label line
      XSIZ = 1.0
      YSIZ =  .67
      CALL JSIZE(XSIZ,YSIZ)
      DELZ = 1.
      CALL J3MOVE(TX,TY,Z2+DELZ)
      CALL JJUST(2,2)
      CALL J1STRG(TITLE)
  100 CONTINUE
C---
C-
  999 RETURN
      END
