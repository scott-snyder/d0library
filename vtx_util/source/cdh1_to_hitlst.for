      SUBROUTINE CDH1_TO_HITLST(LABEL,NHITS,HITLST, MAXHITS, ISTAT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Unpack data from level 2 compressed hits bank CDH1 
C-                         to HITLST array (eventually to VWDA and VSEC).
C-
C-   Inputs  : LABEL        bit-packed location on VTX LAYER, SECTOR, WIRE
C-             MAXHITS      number of hits allowed for the rest of the  
C-                          sector to which the channel LABEL belongs to      
C-   Outputs : HITLST(*)    data array for the hits 
C_             NHITS        number of hits found on this wire_end         
C-             ISTAT = 0    if data found for LAYER, SECTOR, else = -1
C-
C-   Created  14-FEB-1994   Liang-ping Chen after Peter Grudberg's VCHT_UNPACK
C-   Updated  28-NOV-1994   Liang-ping Chen replace 1-word UCOPY with EQUIVALENCE   
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      INTEGER LAYER, SECTOR, WIRE, ISTAT
      INTEGER MXHTOT
      PARAMETER (MXHTOT=500)
      REAL    HITLST(8,*)
C
      REAL LEAST_COUNT
      INTEGER LCDH1, GZCDH1
      INTEGER CDH1_LENGTH, NHEAD, NWDSHT, OFFSET, NSEC_END
      INTEGER RUN, RUNSAV, EVT, EVTSAV
      INTEGER SECTOR_HEADER, SEC, LAY, WORDCOUNT
      INTEGER SECID_OFFSET, COUNT_OFFSET, COUNT_LENGTH
      INTEGER POINTER(0:2,0:31,0:1), NHVWDA
      INTEGER NHITS, IHIT, END, ADDR_OFFSET
      INTEGER ITIME_OFFSET, ITIME_LENGTH
      INTEGER PEAK_OFFSET, PEAK_LENGTH
      INTEGER NHVSEC_OFFSET, NHVSEC_LENGTH
      INTEGER STAT_OFFSET, STAT_LENGTH
      INTEGER ISTATUS, JSTATUS,ADDR, IER
      INTEGER LABEL, MAXHITS, ADDRFLG
      INTEGER KPVPDL, GZVPDL, IPED
      REAL    FPED 
      REAL    AREA, RTIME
      REAL    BILIPT
      INTEGER LOC_LABEL, LOC_RAWW, LOC_JSTATUS 
      REAL    R_LABEL, R_RAWW, R_JSTATUS
      EQUIVALENCE ( LOC_LABEL, R_LABEL ), (LOC_RAWW,R_RAWW), 
     &            ( LOC_JSTATUS, R_JSTATUS) 
      LOGICAL FIRST
C
C ****  CDH1 packing parameters:
C
      PARAMETER ( COUNT_OFFSET = 0 )
      PARAMETER ( COUNT_LENGTH = 13 )
      PARAMETER ( NHVSEC_OFFSET = 13 )
      PARAMETER ( NHVSEC_LENGTH = 11 )
      PARAMETER ( SECID_OFFSET = 24 )
      PARAMETER ( PEAK_OFFSET = 0 )
      PARAMETER ( PEAK_LENGTH = 10 )
      PARAMETER ( STAT_OFFSET = 10 )
      PARAMETER ( STAT_LENGTH = 3 )
      PARAMETER ( ADDR_OFFSET = 13 )
      PARAMETER ( ITIME_OFFSET = 17 )
      PARAMETER ( ITIME_LENGTH = 15 )
C
      DATA RUNSAV, EVTSAV / -1, -1 /
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF ( FIRST ) THEN
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGET('BILIPT',BILIPT,IER)
        FIRST=.FALSE.
      ENDIF
      ISTAT = -1
      LCDH1 = GZCDH1()
      IF ( LCDH1 .LE. 0 ) GO TO 999
C
C ****  On the first call for a given event, spin through CDH1 and build pointer
C ****  tables
C
      CALL EVNTID(RUN,EVT)
      IF ( RUN .NE. RUNSAV .OR. EVT .NE. EVTSAV ) THEN
        RUNSAV = RUN
        EVTSAV = EVT
        CALL VZERO(POINTER,3*32*2)
        CDH1_LENGTH = IQ(LCDH1-1)
        NHEAD = IQ(LCDH1+2)
        NWDSHT = IQ(LCDH1+4)
        LEAST_COUNT = Q(LCDH1+8)
        OFFSET = NHEAD + 1
        NSEC_END = 0
         DO WHILE ( OFFSET .LE. CDH1_LENGTH)
          SECTOR_HEADER = IQ(LCDH1+OFFSET)
          SEC = IBITS(SECTOR_HEADER,SECID_OFFSET  ,5)
          LAY = IBITS(SECTOR_HEADER,SECID_OFFSET+5,2)
          END = IBITS(SECTOR_HEADER,SECID_OFFSET+7,1)
          POINTER(LAY,SEC,END) = OFFSET
          WORDCOUNT = IBITS(SECTOR_HEADER,COUNT_OFFSET,COUNT_LENGTH)
          IF (WORDCOUNT .EQ.0. ) THEN  ! should never happen, but, break the 
                                       ! possible infinete loop 
            GOTO 222
          ENDIF
          OFFSET = OFFSET + WORDCOUNT
          NSEC_END = NSEC_END + 1
        ENDDO
      ENDIF

222   CONTINUE

      NHITS=0                                   
      LAYER = IBITS(LABEL, 9, 2)
      SECTOR= IBITS(LABEL, 4, 5)
      END   = IBITS(LABEL, 0, 1)  

C
C ****  Find bank offset of sector in question
C     
      OFFSET = POINTER(LAYER,SECTOR, END)
      IF ( OFFSET .EQ. 0 ) GO TO 999
      ISTAT = 0   ! There is data for this sector at this END
C
      SECTOR_HEADER = IQ(LCDH1+OFFSET)
      WORDCOUNT = IBITS(SECTOR_HEADER,COUNT_OFFSET,COUNT_LENGTH)
      NHVWDA = (WORDCOUNT-1)/NWDSHT   ! # of VWDA hits
C
C **** find the pedastal value for channel LABEL, to be used for status bits
C
      WIRE   = IBITS( LABEL, 1, 3 )
      KPVPDL = GZVPDL(LAYER)
      KPVPDL = KPVPDL + ( SECTOR*IC(KPVPDL+4) + 2*WIRE + END )
     &             * IC( KPVPDL+3 ) + 5
      FPED  = C( KPVPDL+1 )            ! Pedestal
      IPED = NINT( FPED )
C
C ****  Spin through data in CDH1 and fill HITLST array                         
C ****  (eventually to VWDA bank)
C
      ADDRFLG=0      
      IHIT = 1
      OFFSET = OFFSET + 1

      DO WHILE ( IHIT.LE. NHVWDA )
        ADDR   =        IBITS(IQ(LCDH1+OFFSET),13, 4) 
        IF (ADDR.EQ. IBITS(LABEL, 0, 4) .AND. NHITS .LT. MAXHITS) THEN
          NHITS=NHITS+1  
          ADDRFLG=1  ! the right wire_end has been encontered at least once
          AREA   =  FLOAT(IBITS(IQ(LCDH1+OFFSET), 0,10))
          ISTATUS=        IBITS(IQ(LCDH1+OFFSET),10, 3) 
          RTIME   =  FLOAT(IBITS(IQ(LCDH1+OFFSET),17,15))*LEAST_COUNT

          JSTATUS=0
          CALL MVBITS(ISTATUS,0,1,JSTATUS, 0)
          CALL MVBITS(ISTATUS,1,2,JSTATUS, 2)
          IF (IBITS(IQ(LCDH1+OFFSET+1), 8,8)-IPED.GE.INT(BILIPT)) THEN 
            JSTATUS= IBSET(JSTATUS, 1)
          ENDIF           
          LOC_LABEL = LABEL         ! Get local copy for equivalence
          HITLST(1,NHITS)=R_LABEL
          HITLST(2,NHITS)= RTIME 
          HITLST(3,NHITS)= AREA 
          HITLST(4,NHITS)= 0.      
          HITLST(5,NHITS)= HITLST(3,NHITS)
          LOC_RAWW = IQ(LCDH1+OFFSET+1)
          HITLST(6,NHITS)=R_RAWW
          HITLST(7,NHITS)= SQRT(HITLST(3,NHITS))
          LOC_JSTATUS     = JSTATUS
          HITLST(8,NHITS) = R_JSTATUS
        ELSEIF (ADDRFLG.EQ.1) THEN  ! next wire_end
          GOTO 999
        ENDIF
        IHIT=IHIT+1
        OFFSET=OFFSET+2 
      ENDDO   ! Loop over CDH1 hits           
C
  999 RETURN
      END
