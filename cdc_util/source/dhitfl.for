      SUBROUTINE DHITFL(LAYER,SECTOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : compress one DSEC/DCDA bank information into
C-                         DHIT bank
C-
C-   Inputs  : LAYER, SECTOR
C-   Outputs : none
C-
C-   Entry point: DHITPT(LAYER,SECTOR,JPOINT,NH)
C-   Inputs  : LAYER, SECTOR
C-   Outputs : JPOINT: pointer for the start address of the hits in 
C-                     this LAYER and SECTOR in DHIT bank
C-             NH: total number of hits in this LAYER and this SECTOR        
C-
C-   Created  11-JUL-1991   Qizhong Li-Demarteau
C-   Updated   1-NOV-1991   Qizhong Li-Demarteau  added rebuilding pointer
C-                                                map in DHITPT if needed 
C-   Updated   4-MAR-1992   Qizhong Li-Demarteau  use BYTE_ORDER.PARAMS 
C-   Updated  13-Nov-1993   C. Klopfenstein use 3 words/hit instead of 2,
C-                          to allow RECO to run from DHIT
C-   Updated  14-Nov-1993   C. Klopfenstein include error on z
C-   Updated   8-APR-1994   Qizhong Li-Demarteau  store Ionisation of hit
C-                                            (MIP) instead of pulse area 
C-   Updated  12-APR-1994   Srini Rajagopalan  DHIT bank contains only two bits
C-   for delay line quality, each for one side of delay line. This bit is set if
C-   the delay line hit exist AND is not saturated AND is not overallaped.
C-   If ANY of this condition fails, the bit (20 and 21) is set zero. 
C-             >>> Also Gets DL Status from only two bits in DCDA bank
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CDPARA.INC'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
      INTEGER LAYER, SECTOR
      INTEGER LDHIT, GZDHIT, LDSEC, GZDSEC, IP, POINT, GZDTMW, IPSW
      INTEGER NHIT, IHIT, NWHIT, NWORDS, WIRE, LENGTH, LDIF
      INTEGER LABEL0, LABEL1, LABEL2
      INTEGER LDCDA, GZDCDA, LDTMW, STAT1, IPTM, IPOLD, HTDATA
      INTEGER IPOINT(0:3,0:31), TOTHIT(0:3,0:31), JPOINT, NH
      INTEGER RUNSAV, IDSAV, RUN, ID
      INTEGER NDHIT, KLAY, KSEC, NLAY, NSEC, KHIT, KP
      INTEGER CMPRSS
      PARAMETER( CMPRSS = 2**12 )
      INTEGER*2 HTINFO(2)
      EQUIVALENCE (HTINFO(1),HTDATA)
      integer*2 ShortData(2)
      integer WordData
      equivalence (ShortData(1), WordData)
      REAL TZERO, TEMP1, TEMP2, TEMP3, temp4, MAXLEN
      PARAMETER( MAXLEN = 32767.0 )
C
      SAVE RUNSAV, IDSAV
      DATA RUNSAV,IDSAV/-1,-1/
C
C----------------------------------------------------------------------
C
      CALL EVNTID(RUN,ID)
      IF (RUN .NE. RUNSAV .OR. ID .NE. IDSAV) THEN
        RUNSAV = RUN
        IDSAV = ID
        CALL VZERO(IPOINT(0,0),4*32)
        CALL VZERO(TOTHIT(0,0),4*32)
      ENDIF
C
      LDHIT = GZDHIT()
      IF (LDHIT .LE. 0) CALL BKDHIT(LDHIT)
      NWORDS = IQ(LDHIT+3)
      LENGTH = IQ(LDHIT-1) - 3 - IQ(LDHIT+2) * NWORDS
      LDSEC = GZDSEC(SECTOR,LAYER)
      IF (LDSEC .LE. 0) GOTO 999
      LDCDA = GZDCDA(SECTOR,LAYER)
      IF (LDCDA .LE. 0) GOTO 999
      NHIT = IQ(LDSEC + 1)
      TOTHIT(LAYER,SECTOR) = NHIT
      LDIF = LENGTH - NWORDS*NHIT
      IF (LDIF .LT. 0) THEN
        CALL MZPUSH(IXCOM,LDHIT,0,-LDIF,'I')
        LDSEC = GZDSEC(SECTOR,LAYER)
        LDCDA = GZDCDA(SECTOR,LAYER)
      ENDIF
      LDTMW = GZDTMW(LAYER)
      IF (LDTMW .LE. 0) GOTO 999
C
      IPOINT(LAYER,SECTOR) = 3 + IQ(LDHIT + 2) * NWORDS
      IP = LDHIT + IPOINT(LAYER,SECTOR)
      LABEL0 = LAYER*2**16 + SECTOR*2**11
      DO 100 WIRE = 0, MXSENS
        LABEL1 = LABEL0 + WIRE*2**8
        NWHIT = IQ(LDSEC + 4 + WIRE)
        POINT = IQ(LDSEC + 4 + IQ(LDSEC+2) + WIRE)
        IPTM = LDTMW + (SECTOR*IC(LDTMW+4)+WIRE)*IC(LDTMW+3) + 4
        TZERO = C(IPTM + 1)
        DO 200 IHIT = 1, NWHIT
          LABEL2 = LABEL1 + IHIT*2
          IQ(IP+1) = LABEL2             ! logical address of the hit          
          IQ(LDHIT + 2) = IQ(LDHIT + 2) + 1
          IPOLD = LDSEC + POINT
          IPSW = LDCDA + IQ(IPOLD+10)
          STAT1 = IQ(IPSW + 8)
          CALL MVBITS(STAT1,0,2,IQ(IP+1),18)
          TEMP1 = (Q(IPSW + 2) - TZERO) * 10
          IF (TEMP1 .GT. MAXLEN) TEMP1 = MAXLEN
          HTINFO(WORD1) = NINT(TEMP1)
                              ! store (Drift time - T0) (unit: 0.1ns)
          WordData = 0
          IF (WIRE .EQ. 0 .OR. WIRE .EQ. 6) THEN
            IF (Q(IPOLD+6) .LT. 999.9) THEN
              TEMP2 = Q(IPOLD + 4) * 100      
              IF (TEMP2 .GT. MAXLEN) TEMP2 = MAXLEN
              HTINFO(WORD2) = NINT(TEMP2)
              ! Store Z position (unit: 0.1mm)
            ELSE
              HTINFO(WORD2) = 9999       ! no Z information on the outer SW
            ENDIF
            IF (IQ(IPOLD + 11) .GT. 0) THEN
              STAT1 = IBITS(IQ(LDCDA + IQ(IPOLD+11) + 8), 0, 2)
              if (stat1 .eq. 0) then
                 IQ(IP+1) = ibset(IQ(IP+1),20)
C                CALL MVBITS(STAT1,1,1,IQ(IP+1),20)
              endif
            ENDIF
            IF (IQ(IPOLD + 12) .GT. 0) THEN
              STAT1 = IBITS(IQ(LDCDA + IQ(IPOLD+12) + 8), 0, 2)
              if (stat1 .eq. 0) then
                 IQ(IP+1) = ibset(IQ(IP+1),21)
C                CALL MVBITS(STAT1,1,1,IQ(IP+1),21)
              endif
            ENDIF
C put pulse area in 2nd word for outer SW
            TEMP3 = Q(IPOLD+7)*100
            IF (TEMP3 .GT. MAXLEN) TEMP3 = MAXLEN
            ShortData(Word1) = temp3
C  store error in Z position
            TEMP4 = Q(IPOLD + 6) * 1000
            IF (TEMP4 .GT. MAXLEN) TEMP4 = MAXLEN
            ShortData(Word2) = temp4
          ELSE
            TEMP3 = Q(IPOLD+7)*100
            IF (TEMP3 .GT. MAXLEN) TEMP3 = MAXLEN
            HTINFO(WORD2) = NINT(TEMP3)
          ENDIF
          IQ(IP+2) = HTDATA
          IQ(IP+3) = WordData
          IP = IP + NWORDS
          POINT = POINT + IQ(LDSEC+3)
  200   CONTINUE
  100 CONTINUE
C
C  to indicate that the compressed hits bank has been done for this DSEC
C
      IQ(LDSEC) = IOR(IQ(LDSEC),CMPRSS)     
C
  999 RETURN
C
      ENTRY DHITPT(LAYER,SECTOR,JPOINT,NH)
C
      CALL EVNTID(RUN,ID)
      IF (RUN .NE. RUNSAV .OR. ID .NE. IDSAV) THEN
C
C  in case of that DHITFL is not called in this program (e.g. running on
C  a STA input file), rebuild the pointor map if there is a DHIT bank exist
C
        RUNSAV = RUN
        IDSAV = ID
        CALL VZERO(IPOINT(0,0),4*32)
        CALL VZERO(TOTHIT(0,0),4*32)
        CALL PATHRS
        LDHIT = GZDHIT()
        IF (LDHIT .LE. 0) GOTO 300
        NDHIT = IQ(LDHIT+2)
        KP = LDHIT + 3
        NLAY = -1
        NSEC = -1
        DO 301 KHIT = 1, NDHIT
          KLAY = IBITS(IQ(KP+1),16,2)
          KSEC = IBITS(IQ(KP+1),11,5)
          IF (KLAY .NE. NLAY .OR. KSEC .NE. NSEC) THEN
            NLAY = KLAY
            NSEC = KSEC
            IPOINT(KLAY,KSEC) = KP - LDHIT 
          ENDIF
          TOTHIT(KLAY,KSEC) = TOTHIT(KLAY,KSEC) + 1
          KP = KP + IQ(LDHIT+3)
  301   CONTINUE
      ENDIF
  300 JPOINT = IPOINT(LAYER,SECTOR)
      NH = TOTHIT(LAYER,SECTOR)
      RETURN
C
      END
