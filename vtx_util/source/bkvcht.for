      SUBROUTINE BKVCHT(NHITS,LVCHT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book VTX compressed hits bank VCHT.
C-
C-   Inputs  : NHITS = total number of VWDA-type hits
C-   Outputs : LVCHT pointer to VCHT bank
C-
C-   Created  26-OCT-1993   Peter Grudberg
C-   Updated  11-FEB-1994   Ed Oltman  VERSION 1 : 1 WORD/HIT
C-   Updated  14-SEP-1994   Liang-ping Chen replace LEAST_COUNT by TIME_LC
C-                          to avoid confusion with the LEAST_COUNT used for
C-                          CDH1
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZVCHT.LINK'
C
      INTEGER NHITS, LVCHT
C
      REAL TIME_LC,T_OFFSET
      REAL WEIGHT(2), WEIGHT_BITVAL, WINDOW, WINDOW_BITVAL
      INTEGER LVTXH, GZVTXH, ISETVN
      INTEGER NSECT, NHEAD, NWDSHT, TABLE_VERSION
      INTEGER NL, NS, ND, NIO, IER
      INTEGER N_BEFORE, N_AFTER, BANK_VERSION
      INTEGER THR1(2), THR2(2), THR3(2)
      INTEGER IWEIGHT, IWINDOW, VCHT_PACK_HITINFO
      LOGICAL FIRST
      DATA FIRST / .TRUE. /
      DATA NL, NS / 0, 0 /
      DATA NSECT, NHEAD, NWDSHT / 80, 8, 1 /
      DATA BANK_VERSION / 1 /
      DATA TIME_LC / 0.60 /
      DATA T_OFFSET    / 50.  /
      DATA WEIGHT_BITVAL, WINDOW_BITVAL / 0.05, 1.0 /
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGET_i('TABLE_VERSION',TABLE_VERSION,IER)
        IF ( IER .NE. 0 ) TABLE_VERSION = 1
        CALL EZGET_iarr('PULTH1',THR1,IER)
        CALL EZGET_iarr('PULTH2',THR2,IER)
        CALL EZGET_iarr('PULTH3',THR3,IER)
        CALL EZGET_rarr('PULWEI',WEIGHT,IER)
        CALL EZGET('WINDOW',WINDOW,IER)
        CALL EZGET_i('BINS_BEFORE_PEAK',N_BEFORE,IER)
        N_AFTER = 3 - N_BEFORE
        CALL EZRSET
        CALL MZFORM('VCHT','5I 3F -B',NIO)
      ENDIF
C
      LVTXH = GZVTXH()
      IF ( LVTXH .GE. 0 ) THEN
        ND = NHEAD + NSECT + NWDSHT*NHITS
        CALL MZBOOK(IXMAIN,LVCHT,LVTXH,-IZVCHT,'VCHT',NL,NS,ND,NIO,0)
      ELSE
        CALL ERRMSG('VTXH does not exist','BKVCHT',
     &    'Cannot book VCHT - no supporting bank','F')
        LVCHT = 0
        GO TO 999
      ENDIF
C
C ****  Fill header info (do not fill the NHITS word (word 3) - fill that when
C ****  the hits are actually stored).
C
      IQ(LVCHT) = ISETVN(IQ(LVCHT),BANK_VERSION)
      IQ(LVCHT+1) = BANK_VERSION
      IQ(LVCHT+2) = NHEAD
      IQ(LVCHT+4) = NWDSHT
C
C ****  Build hitfinding info word
C
      IWINDOW = NINT(WINDOW/WINDOW_BITVAL)
      IWEIGHT = NINT(WEIGHT(1)/WEIGHT_BITVAL)
      IQ(LVCHT+5) =  VCHT_PACK_HITINFO(IWINDOW,IWEIGHT,
     &  THR1(1),THR2(1),THR3(1),TABLE_VERSION)
C
      Q(LVCHT+6)  = T_OFFSET
      Q(LVCHT+7)  = 0.
      Q(LVCHT+8)  = TIME_LC
C
  999 RETURN
      END
