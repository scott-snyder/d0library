      SUBROUTINE VCHT_UNPACK(LAYER,SECTOR,ISTAT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Unpack data from compressed hits bank VCHT to full
C-                         hit banks VSEC and, if requested, VWDA
C-
C-   Inputs  : LAYER, SECTOR
C-   Outputs : ISTAT = 0 if data found for LAYER, SECTOR, else = -1
C-
C-   Created  31-OCT-1993   Peter Grudberg
C-   Updated   2-DEC-1993   Liang-Ping Chen, Al Clark fill bit 2,3 of STAT
C-                          with status bits of VCHT and pass it to
C-                          VTX_COMBINE so VSECHIT(10) can be fully filled
C-   Updated  10-FEB-1994   Ed Oltman  BOOK AND FILL SOME VWDA IF DESIRED
C-   Updated  12-FEB-1994   Ed Oltman  Add VCHT version dependance
C-   Updated   5-MAR-1994   Ed Oltman  refresh VCHT pointer after CALL BK.... 
C-   Updated   1-July-1994  Danilo Puseljic add call to VTX_DYNADJ,
C-                          VTX_REFRESH_LINKS, VTX_CLRLNK_VSEC,
C-                          and a check on the presence of VTRAKS_RCP
C-   Updated  11-JUL-1994   liang-ping chen check VERSION number of VTMW
C-   Updated  18-JUL-1994   Liang-ping Chen refresh LVCHT after VTX_DYNADJ as
C-                                          well 
C-   Updated  14-SEP-1994   Liang-ping Chen replace LEAST_COUNT by TIME_LC
C-                          to avoid confusion with the LEAST_COUNT used for
C-                          CDH1
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      INTEGER LAYER, SECTOR, ISTAT
C
      REAL TIME_LC, TIME(0:1), PEAK(0:1)
      REAL VSECHIT(10)
      INTEGER LVCHT, GZVCHT, IE
      INTEGER VCHT_LENGTH, NHEAD, NWDSHT, OFFSET, NSEC
      INTEGER RUN, RUNSAV, EVT, EVTSAV
      INTEGER SECTOR_HEADER, SEC, LAY, WORDCOUNT
      INTEGER SECID_OFFSET, COUNT_OFFSET, COUNT_LENGTH
      INTEGER POINTER(0:2,0:31), NHVSEC, NHVWDA
      INTEGER LVSEC, GZVSEC, NWIRE, NWVSEC, VSEC_OFFS
      INTEGER IHIT, WORD, WIRE, END, ITIME, IRAW(0:1)
      INTEGER NEXT_POINTER, NHVSEC_OFFSET, NHVSEC_LENGTH
      INTEGER STAT(0:1) , NH(0:7)
      INTEGER N_MATCHED, N_TOTAL, N_MINUSZ, N_PLUSZ
      INTEGER LVLAY, GZVLAY, LVTXH, GZVTXH
      LOGICAL MATCHED,FIRST,VWDA_FROM_VCHT
      INTEGER ERR,LVWDA,NCHANS,NWVWDA,VWDA_OFFS,NH1(0:15)
      INTEGER LVGNL,GZVGNL,NVGNL,LVTMW,GZVTMW,NVTMW,VWD_OFF(0:1)
      INTEGER IVWD(500,0:1),LAST_WIRE,NVWDA,STATUS
      REAL    GAIN(0:15),TZERO(0:15),RVWD(500,0:1),T_OFFSET
      EQUIVALENCE (IVWD,RVWD),(STATUS,VSECHIT(10))
      INTEGER VERS,MAX_VERS,LRCP,IER
      PARAMETER (MAX_VERS=1)
      INTEGER MATCH_BIT(0:MAX_VERS) , ADDR_OFFSET(0:MAX_VERS)
      INTEGER ITIME_LENGTH(0:MAX_VERS) , ITIME_OFFSET(0:MAX_VERS)
      INTEGER STAT_LENGTH(0:MAX_VERS) , STAT_OFFSET(0:MAX_VERS)
      INTEGER PEAK_LENGTH(0:MAX_VERS) , PEAK_OFFSET(0:MAX_VERS)
      INTEGER IVERS
C
C ****  VCHT packing parameters: SECTOR HEAD WORD
C
      PARAMETER ( COUNT_OFFSET = 0 )
      PARAMETER ( COUNT_LENGTH = 13 )
      PARAMETER ( NHVSEC_OFFSET = 13 )
      PARAMETER ( NHVSEC_LENGTH = 11 )
      PARAMETER ( SECID_OFFSET = 24 )
C
C ****  VCHT packing parameters: HIT
C
      DATA MATCH_BIT    /31 , 31/
      DATA ITIME_LENGTH /15 , 12/
      DATA ITIME_OFFSET /16 , 19/
      DATA ADDR_OFFSET  /12 , 15/
      DATA STAT_LENGTH  / 2 ,  5/
      DATA STAT_OFFSET  /10 , 10/
      DATA PEAK_LENGTH  /10 , 10/
      DATA PEAK_OFFSET  / 0 ,  0/
      DATA FIRST/.TRUE./
      DATA RUNSAV, EVTSAV / -1, -1 /
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZLOC('VTRAKS_RCP',LRCP)
        IF (LRCP .EQ. 0) THEN
          CALL INRCP('VTRAKS_RCP',IER)
        ENDIF
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGET('VWDA_FROM_VCHT',VWDA_FROM_VCHT,ERR)
        CALL EZRSET
        LVTMW=GZVTMW(0)
        IVERS = IBITS(IC(LVTMW),13,5)
      ENDIF
      ISTAT = -1
      LVCHT = GZVCHT()
      IF ( LVCHT .LE. 0 ) GO TO 999
C
C ****  On the first call for a given event, spin through VCHT and build pointer
C ****  tables
C
      CALL EVNTID(RUN,EVT)
      IF ( RUN .NE. RUNSAV .OR. EVT .NE. EVTSAV ) THEN
        RUNSAV = RUN
        EVTSAV = EVT
        CALL VTX_DYNADJ
        CALL VTX_REFRESH_LINKS
        LVCHT = GZVCHT()
        VCHT_LENGTH = IQ(LVCHT-1)
        VERS  = IQ(LVCHT+1)
        NHEAD = IQ(LVCHT+2)
        NWDSHT = IQ(LVCHT+4)
        T_OFFSET = 0.
        IF (VERS .EQ. 1) T_OFFSET = Q(LVCHT+6)
        TIME_LC = Q(LVCHT+8)
        OFFSET = NHEAD + 1
        NSEC = 0
        CALL VZERO(POINTER,3*32)
        DO WHILE ( OFFSET .LE. VCHT_LENGTH .AND. NSEC .LT. 80 )
          SECTOR_HEADER = IQ(LVCHT+OFFSET)
          SEC = IBITS(SECTOR_HEADER,SECID_OFFSET,5)
          LAY = IBITS(SECTOR_HEADER,SECID_OFFSET+5,2)
          WORDCOUNT = IBITS(SECTOR_HEADER,COUNT_OFFSET,COUNT_LENGTH)
          POINTER(LAY,SEC) = OFFSET
          OFFSET = OFFSET + WORDCOUNT
          NSEC = NSEC + 1
        ENDDO
      ENDIF
C
C ****  Find bank offset of sector in question
C
      OFFSET = POINTER(LAYER,SECTOR)
      IF ( OFFSET .EQ. 0 ) GO TO 999  ! No data for this sector
      ISTAT = 0   ! There is data for this sector
C
      SECTOR_HEADER = IQ(LVCHT+OFFSET)
      WORDCOUNT = IBITS(SECTOR_HEADER,COUNT_OFFSET,COUNT_LENGTH)
      NHVSEC = IBITS(SECTOR_HEADER,NHVSEC_OFFSET,NHVSEC_LENGTH)
      NHVWDA = (WORDCOUNT-1)/NWDSHT   ! # of VWDA hits
C
C ****  Book VSEC (drop old VSEC, if necessary)
C
      LVSEC = GZVSEC(LAYER,SECTOR)
      IF ( LVSEC .GT. 0 ) THEN
        CALL VTX_CLRLNK_VSEC(LAYER,SECTOR)
        CALL MZDROP(IXCOM,LVSEC,' ')
      ENDIF
C
      IF ( NHVSEC .EQ. 0 ) GO TO 999  ! Don't book empty bank
C
      CALL BKVSEC(LAYER,SECTOR,NHVSEC,LVSEC)
      IQ(LVSEC+1) = NHVSEC
      NWIRE  = IQ(LVSEC+2)
      NWVSEC = IQ(LVSEC+3)
      VSEC_OFFS = 3 + 2*NWIRE + 1     ! First word of first hit in VSEC bank
      CALL VZERO(NH,8)                ! Zero VSEC hit counters
      CALL VZERO(NH1(0),16)            ! Zero VWDA hit counters
      IF (VWDA_FROM_VCHT) THEN
        CALL BKVWDA(LAYER,SECTOR,NHVWDA,LVWDA)
        IQ(LVWDA+1) = NHVWDA
        NCHANS = IQ(LVWDA+2)
        NWVWDA = IQ(LVWDA+3)
        CALL VZERO(IQ(LVWDA+4),2*NCHANS)
        VWDA_OFFS = 3 + 2*NCHANS + 1  ! First word of first hit in VWDA bank
        CALL VZERO(VWD_OFF(0),2)
        LVGNL = GZVGNL(LAYER)
        NVGNL= IC(LVGNL+3)
        IF (NVGNL .NE. 1) CALL ERRMSG('VGNL structure unexpected',
     &    'VCHT_UNPACK',' ','F')
        CALL UCOPY(C(LVGNL+16*SECTOR+6+3*41),GAIN(0),16)
      ENDIF
      LVCHT = GZVCHT()
      IF (VERS .EQ. 1 .OR. VWDA_FROM_VCHT) THEN
        LVTMW = GZVTMW(LAYER)
        NVTMW = IC(LVTMW+3)
        IF (IVERS.EQ.0)  THEN   ! VTMW with IVERS=0 is used for MC
          DO WIRE = 0,7
            DO END = 0,1
              TZERO(2*WIRE+END) = C(LVTMW+(8*SECTOR+WIRE)*NVTMW+6)
            ENDDO
          ENDDO 
        ELSEIF (IVERS.EQ.1) THEN! VTMW with IVERS=1 is used for DATA
          DO WIRE = 0,7
            DO END = 0,1
              TZERO(2*WIRE+END) = C(LVTMW+(8*SECTOR+WIRE)*NVTMW+6+2*END)
            ENDDO
          ENDDO
        ELSE
          CALL ERRMSG('Invalid VTMW version','VCHT_UNPACK',
     &      'codes probably need to be updated','F')
        ENDIF
      ENDIF
C
C ****  Spin through data in VCHT and fill VSEC
C
      IHIT = 1
      N_MATCHED = 0
      N_MINUSZ = 0
      N_PLUSZ = 0
      OFFSET = OFFSET + 1
      LAST_WIRE = 0
C
C ****  Loop over all hits in the sector.  Include 1 extra pass for the benefit
C ****  of filling VWDA, if requested
C
      DO WHILE ( IHIT .LE. NHVWDA+1 )
        IF (IHIT .LE. NHVWDA) THEN
          WORD = IQ(LVCHT+OFFSET)
          WIRE = IBITS(WORD,ADDR_OFFSET(VERS)+1,3)
        ELSE
          WIRE = 8
        ENDIF
        IF (VWDA_FROM_VCHT .AND. (WIRE .NE. LAST_WIRE)) THEN
          DO IE = 0,1
            NVWDA = NH1(2*LAST_WIRE+IE)
            IQ(LVWDA+4+2*LAST_WIRE+IE) = NVWDA
            IF (NVWDA .GT. 0) THEN
              CALL UCOPY(RVWD(1,IE),Q(LVWDA+VWDA_OFFS),NVWDA*NWVWDA)
              IQ(LVWDA+4+NCHANS+2*LAST_WIRE+IE) = VWDA_OFFS
              VWDA_OFFS = VWDA_OFFS + NVWDA*NWVWDA
            ENDIF
            VWD_OFF(IE) = 0
          ENDDO
          LAST_WIRE = WIRE
        ENDIF
        IF (IHIT .GT. NHVWDA) GO TO 10
C
C ****  Is this a matched hit?  If so, combine it with the following hit.
C ****  Otherwise, form an unmatched VSEC hit.  Note that for a matched hit, end
C ****  0 data always comes first.
C
        CALL VZERO(IRAW(0),2)
        MATCHED = BTEST(IQ(LVCHT+OFFSET),MATCH_BIT(VERS))
        IF ( MATCHED ) THEN
          N_MATCHED = N_MATCHED + 1
          IHIT = IHIT + 2   ! 2 VWDA words used for a matched VSEC hit
          DO IE = 0, 1
            WORD = IQ(LVCHT+OFFSET+NWDSHT*IE)
            ITIME = IBITS(WORD,ITIME_OFFSET(VERS),ITIME_LENGTH(VERS))
            TIME(IE) = FLOAT(ITIME)*TIME_LC
            PEAK(IE) = IBITS(WORD,PEAK_OFFSET(VERS),PEAK_LENGTH(VERS))
            STAT(IE) = IBITS(WORD,STAT_OFFSET(VERS),STAT_LENGTH(VERS))
            IF (VERS .EQ. 0) THEN
              STAT(IE) = 4*STAT(IE)              ! VWDA status word bits 2,3
              IRAW(IE) = IQ(LVCHT+OFFSET+2*IE+1)
            ELSE
              STAT(IE) = IBITS(STAT(IE),0,1)     ! VWDA status word bit 0
              TIME(IE) = TIME(IE) - T_OFFSET + TZERO(2*WIRE+IE)
            ENDIF
            NH1(2*WIRE+IE) = NH1(2*WIRE+IE) + 1
            IF (VWDA_FROM_VCHT) THEN
              IVWD(VWD_OFF(IE)+1,IE) = 2*WIRE + IE
              RVWD(VWD_OFF(IE)+2,IE) = TIME(IE)
              RVWD(VWD_OFF(IE)+3,IE) = PEAK(IE)/GAIN(2*WIRE+IE)
              RVWD(VWD_OFF(IE)+4,IE) = 0.               ! PULSE WIDTH
              RVWD(VWD_OFF(IE)+5,IE) = PEAK(IE)
              IVWD(VWD_OFF(IE)+6,IE) = IRAW(IE)
              RVWD(VWD_OFF(IE)+7,IE) =
     &                    SQRT(PEAK(IE)/GAIN(2*WIRE+IE))! AREA ERROR
              IVWD(VWD_OFF(IE)+8,IE) = STAT(IE)         ! STATUS
            ENDIF
          ENDDO
          NH(WIRE) = NH(WIRE) + 1
          END = 0
          IF ( PEAK(1) .GT. PEAK(0) ) END = 1
          OFFSET = OFFSET + 2*NWDSHT
        ELSE
          IHIT = IHIT + 1
          WORD = IQ(LVCHT+OFFSET)
          END  = IBITS(WORD,ADDR_OFFSET(VERS),1)
          IF ( END .EQ. 0 ) THEN
            N_MINUSZ = N_MINUSZ + 1
          ELSE
            N_PLUSZ = N_PLUSZ + 1
          ENDIF
          WIRE = IBITS(WORD,ADDR_OFFSET(VERS)+1,3)
          NH(WIRE) = NH(WIRE) + 1
          ITIME = IBITS(WORD,ITIME_OFFSET(VERS),ITIME_LENGTH(VERS))
          TIME(END) = FLOAT(ITIME)*TIME_LC
          TIME(1-END) = 0.
          PEAK(END) = IBITS(WORD,PEAK_OFFSET(VERS),PEAK_LENGTH(VERS))
          PEAK(1-END) = 0.
          STAT(END) = IBITS(WORD,STAT_OFFSET(VERS),STAT_LENGTH(VERS))
          STAT(1-END) = 0
          IF (VERS .EQ. 0) THEN
            STAT(END) = 4*STAT(END)
            IRAW(END) = IQ(LVCHT+OFFSET+1)
            IRAW(1-END) = 0
          ELSE
            STAT(END) = IBITS(STAT(END),0,1)
            STAT(1-END) = 0
            TIME(END) = TIME(END) - T_OFFSET + TZERO(2*WIRE+END)
          ENDIF
          NH1(2*WIRE+END) = NH1(2*WIRE+END) + 1
          IF (VWDA_FROM_VCHT) THEN
            IVWD(VWD_OFF(END)+1,END) = 2*WIRE + END
            RVWD(VWD_OFF(END)+2,END) = TIME(END)
            RVWD(VWD_OFF(END)+3,END) = PEAK(END)/GAIN(2*WIRE+END)
            RVWD(VWD_OFF(END)+4,END) = 0.               ! PULSE WIDTH
            RVWD(VWD_OFF(END)+5,END) = PEAK(END)
            IVWD(VWD_OFF(END)+6,END) = IRAW(END)
            RVWD(VWD_OFF(END)+7,END) =
     &                  SQRT(PEAK(END)/GAIN(2*WIRE+END))! AREA ERROR
            IVWD(VWD_OFF(END)+8,END) = STAT(END)        ! STATUS
          ENDIF
          OFFSET = OFFSET + NWDSHT
        ENDIF
C
C ****  Build words to be stored in VSEC.
C
        CALL VTX_COMBINE(LAYER,SECTOR,WIRE,END,MATCHED,TIME,
     &    PEAK,STAT,IRAW,VSECHIT)
        IF (PEAK(0) .GT. 0.) THEN
          CALL MVBITS(NH1(2*WIRE  ),0,8,STATUS,16)
          CALL MVBITS(STATUS, 8,4,IVWD(VWD_OFF(0)+8,0),0)
          VWD_OFF(0) = VWD_OFF(0) + NWVWDA
        ENDIF
        IF (PEAK(1) .GT. 0.) THEN
          CALL MVBITS(NH1(2*WIRE+1),0,8,STATUS,24)
          CALL MVBITS(STATUS,12,4,IVWD(VWD_OFF(1)+8,1),0)
          VWD_OFF(1) = VWD_OFF(1) + NWVWDA
        ENDIF
C
        CALL UCOPY(VSECHIT,Q(LVSEC+VSEC_OFFS),NWVSEC)
        VSEC_OFFS = VSEC_OFFS + NWVSEC
C
      ENDDO   ! Loop over VCHT hits
   10 CONTINUE
C
C ****  Fill VSEC header area
C
      NEXT_POINTER = 4 + 2*NWIRE
      DO WIRE = 0, NWIRE - 1
        IQ(LVSEC+4+WIRE) = NH(WIRE)
        IF ( NH(WIRE) .GT. 0 ) THEN
          IQ(LVSEC+4+NWIRE+WIRE) = NEXT_POINTER
          NEXT_POINTER = NEXT_POINTER + NWVSEC*NH(WIRE)
        ELSE
          IQ(LVSEC+4+NWIRE+WIRE) = 0
        ENDIF
      ENDDO
C
C ****  Update hit counters in VTXH, VLAY
C
      N_TOTAL = N_MATCHED + N_MINUSZ + N_PLUSZ
      LVTXH = GZVTXH()
      IQ(LVTXH+1) = IQ(LVTXH+1) + N_TOTAL ! Total hits
      IQ(LVTXH+2) = IQ(LVTXH+2) + N_TOTAL ! Wire hits
      LVLAY = GZVLAY(LAYER)
      IQ(LVLAY+1) = IQ(LVLAY+1) + N_TOTAL
      IQ(LVLAY+2) = IQ(LVLAY+2) + N_MATCHED
      IQ(LVLAY+3) = IQ(LVLAY+3) + N_PLUSZ
      IQ(LVLAY+4) = IQ(LVLAY+4) + N_MINUSZ
C
  999 RETURN
      END
