      SUBROUTINE VWDA_TO_VCHT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Transfer data from VWDA banks to the VCHT compressed
C-                         hit banks.
C-
C-   Inputs  : Data in VWDA
C-   Outputs : Data in VCHT
C-
C-   Created  26-OCT-1993   Peter Grudberg
C-   Updated  30-NOV-1993   Peter Grudberg  Fix ERRMSG call
C-   Updated   1-DEC-1993   Liang-Ping Chen correct a typo. It should be
C-                          IF (BTEST(..), not IF (IBSET(..), before ISTAT
C-   Updated  12-FEB-1994   Ed Oltman Modify for VERSION = 1
C-   Updated  11-JUL-1994   liang-ping chen check VERSION number of VTMW
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$PARAMS:VCHT.PARAMS'
C
      REAL     TIME
      INTEGER LAY, SEC, WIR, END, NSEC(0:2)
      INTEGER N_MATCHED, N_UNMATCHED, N_VWDA
      INTEGER LVLAY, GZVLAY, LVCHT, VCHT_LENGTH
      INTEGER OFFS, PTR, PT, SECID
      INTEGER LVSEC, GZVSEC, LVWDA, GZVWDA
      INTEGER NVSEC, NWIRE, NWVSEC, NVWDA, NCHAN, NWVWDA
      INTEGER WORD_COUNT, NWDSHT
      INTEGER SEC_HEAD, VCHT_PACK_SECHEAD
      INTEGER ADDR, IH, NHWIR, STATUS, HIT(0:1)
      INTEGER IMATCH, ITIME, IPEAK, ISTAT
      INTEGER VCHT_PACK_HIT, EXTEND, EXTRA
      INTEGER SEC_HEAD_OFFS, N_USED
      INTEGER LVTMW,GZVTMW,NVTMW
      INTEGER IVERS 
      LOGICAL FIRST
      REAL    TZERO(0:15)
      DATA NSEC / 16, 32, 32 /
      DATA FIRST / .TRUE. /

C----------------------------------------------------------------------
C
C ****  Loop over VLAY banks; determine number of VWDA hits (number of VWDA hits
C ****  equals 2*N_matched + N_unmatched)
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        LVTMW=GZVTMW(0)
        IVERS = IBITS(IC(LVTMW),13,5)
      ENDIF
      N_MATCHED = 0
      N_UNMATCHED = 0
      DO LAY = 0, 2
        LVLAY = GZVLAY(LAY)
        IF ( LVLAY .GT. 0 ) THEN
          N_MATCHED = N_MATCHED + IQ(LVLAY+2)
          N_UNMATCHED = N_UNMATCHED + IQ(LVLAY+3) + IQ(LVLAY+4)
        ENDIF
      ENDDO
      N_VWDA = 2*N_MATCHED + N_UNMATCHED
      IF ( N_VWDA .EQ. 0 ) GO TO 999
C
C ****  Book VCHT bank with enough room for N_VWDA hits
C
      CALL BKVCHT(N_VWDA,LVCHT)
      VCHT_LENGTH = IQ(LVCHT-1)
      OFFS = IQ(LVCHT+2)          ! Header length in VCHT
      NWDSHT = IQ(LVCHT+4)
C
C ****  Loop over sectors in order, moving data from VWDA to VCHT
C
      N_USED = 0
      DO LAY = 0, 2
        SECID = LAY*32
        DO SEC = 0, NSEC(LAY) - 1
          LVSEC = GZVSEC(LAY,SEC)
          IF ( LVSEC .GT. 0 ) LVWDA = GZVWDA(LAY,SEC)
          IF ( LVSEC .GT. 0 .AND. LVWDA .GT. 0 ) THEN
            NVSEC = IQ(LVSEC + 1)
            NWIRE = IQ(LVSEC + 2)
            NWVSEC = IQ(LVSEC + 3)
            NVWDA = IQ(LVWDA + 1)
            NCHAN = IQ(LVWDA + 2)
            NWVWDA = IQ(LVWDA+3)
            LVTMW = GZVTMW(LAY)
            NVTMW = IC(LVTMW+3)
            IF (IVERS.EQ.0)  THEN   ! VTMW with IVERS=0 is used for MC
              DO WIR = 0,7
                DO END = 0,1
                  TZERO(2*WIR+END) = C(LVTMW+(8*SEC+WIR)*NVTMW+6)
                ENDDO
              ENDDO
            ELSEIF (IVERS.EQ.1) THEN! VTMW with IVERS=1 is used for DATA
              DO WIR = 0,7
                DO END = 0,1
                  TZERO(2*WIR+END) = C(LVTMW+(8*SEC+WIR)*NVTMW+6+2*END)
                ENDDO
              ENDDO
            ELSE
              CALL ERRMSG('Invalid VTMW version','VWDA_TO_VCHT',
     &          'codes probably need to be updated','F')
            ENDIF
            WORD_COUNT = 1    ! Sector head word
            IF ( (OFFS+WORD_COUNT) .GT. VCHT_LENGTH ) THEN
              EXTEND = MAX(NWDSHT*NVWDA+1,1000)
              CALL MZPUSH(IXCOM,LVCHT,0,EXTEND,'R')
              VCHT_LENGTH = IQ(LVCHT-1)
            ENDIF
            SEC_HEAD_OFFS = OFFS
            OFFS = OFFS + 1
            DO WIR = 0, 7
              NHWIR = IQ(LVSEC+4+WIR)
              PTR = IQ(LVSEC+4+NWIRE+WIR)
              DO IH = 1, NHWIR
                STATUS = IQ(LVSEC+PTR+9)
                HIT(0) = IBITS(STATUS,16,8)
                HIT(1) = IBITS(STATUS,24,8)
                IF ( HIT(0) .GT. 0 .AND. HIT(1) .GT. 0 ) THEN
                  IMATCH = 1
                ELSE
                  IMATCH = 0
                ENDIF
                DO END = 0, 1
                  IF ( HIT(END) .GT. 0 ) THEN
                    PT = IQ(LVWDA+4+NCHAN+2*WIR+END)
                    PT = PT + NWVWDA*(HIT(END)-1)
                    TIME = Q(LVWDA+PT+1) - TZERO(2*WIR+END) + TIME_OFF
                    ITIME = MAX0(0,MIN0(2**NBITTIME-1,
     &                                    NINT(TIME/TIME_LC)))
                    IPEAK = MAX0(0,MIN0(2**NBITPEAK-1,
     &                                    NINT(Q(LVWDA+PT+4))))
                    ADDR = 2*WIR + END
                    ISTAT = IBITS(IQ(LVWDA+PT+7),0,1)
                    IQ(LVCHT+OFFS+1) =
     &                VCHT_PACK_HIT(IMATCH,ITIME,ADDR,ISTAT,IPEAK)
                    OFFS = OFFS + NWDSHT
                    WORD_COUNT = WORD_COUNT + NWDSHT
                    N_USED = N_USED + 1
                  ENDIF
                ENDDO         ! Loop over end
                PTR = PTR + NWVSEC
              ENDDO           ! Loop over hit
            ENDDO             ! Loop over wire
            SEC_HEAD = VCHT_PACK_SECHEAD(SECID,NVSEC,WORD_COUNT)
            IQ(LVCHT+SEC_HEAD_OFFS+1) = SEC_HEAD
          ELSE
            NVSEC = 0
            WORD_COUNT = 1
            SEC_HEAD = VCHT_PACK_SECHEAD(SECID,NVSEC,WORD_COUNT)
            IQ(LVCHT+OFFS+1) = SEC_HEAD
            OFFS = OFFS + 1
          ENDIF
          SECID = SECID + 1
        ENDDO
      ENDDO
C
C ****  Check to see if N_VWDA and N_USED match
C
      IF ( N_VWDA .NE. N_USED ) THEN
        CALL ERRMSG('Hit mismatch','VWDA_TO_VCHT',
     &    'Number of hits does not agree','W')
      ENDIF
C
C ****  Store number of hits, get rid of empty space
C
      IQ(LVCHT+3) = N_USED
      EXTRA = VCHT_LENGTH - OFFS
      IF ( EXTRA .GT. 0 ) THEN
        CALL MZPUSH(IXCOM,LVCHT,0,-EXTRA,'R')
      ELSEIF ( EXTRA .LT. 0 ) THEN
        CALL ERRMSG('Bank overwrite','VWDA_TO_VCHT',
     &    'Data written beyond the end of VCHT','F')
      ENDIF
C
  999 RETURN
      END
