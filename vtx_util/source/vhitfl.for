      SUBROUTINE VHITFL(LAYER, SECTOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Store the hits from one VSEC bank in compressed form
C-                         in the VHIT bank.
C-
C-   Inputs  : LAYER, SECTOR
C-   Outputs : fills VHIT
C-
C-      ENTRY VHITST(LABEL) : Set bits for hits on VTXT tracks.
C-      ENTRY VHITPT(LAYER,SECTOR,PNT,NUMHIT): For LAYER, SECTOR, return pointer
C-                                             to first hit and number of hits
C-
C-              Input:  LABEL(0:23) hit info:
C-                              bit 0: Side of cell
C-                              bits 1-7: hit number on wire
C-                              bits 8-17: wire + 8*sector + 256*layer
C-              Output: sets bits in VHIT
C-
C-      ENTRY VHIT_DROP : drop VHIT, zero COUNTR and POINTR arrays
C-
C-   Created  24-AUG-1991   Peter M. Grudberg
C-   Updated  18-OCT-1991   Peter M. Grudberg  Remove VHIT_INI entry, change
C-                              status bits, add VHITPT entry
C-   Updated   9-Mar-1992   Herbert Greenlee
C-      Updated for UNIX (byte order).
C-   Updated  16-Mar-1993   Alexandre Zinchenko - change VHITPT entry as
C-                          in DHITPT
C-   Updated  07-JUN-1993   A. Zinchenko - add NEW_EVENT 
C-   Updated   4-NOV-1993   Peter Grudberg  add VHIT_DROP entry 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:BYTE_ORDER.PARAMS'
C
      INTEGER LAYER, SECTOR
      LOGICAL NEW_EVENT
C
      REAL DRIFT_PLUS, DRIFT_MINUS, ZEE, AREA
      INTEGER LVSEC, GZVSEC, LVHIT, GZVHIT
      INTEGER IP, POINT, NWORDS, NHIT, LENGTH
      INTEGER LABEL0, LABEL1, LABEL2, NWHIT, WIRE, IHIT, STAT
      INTEGER HITDATA(2)
      INTEGER*2 HITINFO(4)
      EQUIVALENCE ( HITDATA(1), HITINFO(1) )
C
      INTEGER POINTR(0:31,0:2), COUNTR(0:31,0:2)
      INTEGER LABEL(0:23), IWIR, HLABEL, LAY, SEC
      INTEGER BITPTN, IHSIDE, ONVTXT, HTSIDE, COMPRESS
      INTEGER RUN, RUNSAV, ID, IDSAV
      INTEGER PNT, NUMHIT
      INTEGER KHIT, KLAY, KSEC, NLAY, NSEC
      PARAMETER ( ONVTXT = 2**23 )
      PARAMETER ( HTSIDE = 1 )
      PARAMETER ( COMPRESS = 12 )
      SAVE RUNSAV, IDSAV
      DATA RUNSAV, IDSAV / -1, -1 /
      DATA NEW_EVENT /.FALSE./
C----------------------------------------------------------------------
C
C ****  Zero arrays for new event
C
      CALL EVNTID(RUN,ID)
      IF ( RUN.NE.RUNSAV .OR. ID.NE.IDSAV ) THEN
        NEW_EVENT = .TRUE.
        RUNSAV=RUN
        IDSAV=ID
        CALL VZERO(POINTR,3*32)
        CALL VZERO(COUNTR,3*32)
      ENDIF
C
      LVSEC = GZVSEC(LAYER,SECTOR)
      IF ( LVSEC .LE. 0 ) GO TO 999     ! No hits in this sector
      NHIT = IQ(LVSEC+1)
      COUNTR(SECTOR,LAYER) = NHIT
C
      LVHIT = GZVHIT()
      IF ( LVHIT .LE. 0 ) CALL BKVHIT(NHIT, LVHIT)
      NWORDS = IQ(LVHIT+3)
      LENGTH = IQ(LVHIT-1) - 3 - IQ(LVHIT+2)*NWORDS
C
C ****  Is there enough room?  If not, make some room.
C
      IF ( LENGTH .LT. NWORDS*NHIT ) THEN
        CALL MZPUSH(IXCOM,LVHIT,0,MAX(NHIT*NWORDS,500),'I')
      ENDIF
C
      POINTR(SECTOR,LAYER) = 3 + IQ(LVHIT+2)*NWORDS
      IP = LVHIT + POINTR(SECTOR,LAYER)
      LABEL0 = LAYER * 2**16 + SECTOR * 2**11
      DO WIRE = 0, 7
        LABEL1 = LABEL0 + WIRE * 2**8
        NWHIT = IQ(LVSEC + 4 + WIRE)
        POINT = LVSEC + IQ(LVSEC + 4 + IQ(LVSEC+2) + WIRE)
        DO IHIT = 1, NWHIT
          LABEL2 = LABEL1 + IHIT*2
          IQ(IP+1) = LABEL2             ! Logical address of hit
          IQ(LVHIT+2) = IQ(LVHIT+2) + 1 ! increment hit counter
C
C ****  Transfer status bits:
C
          STAT = IQ(POINT+9)
          CALL MVBITS(STAT,0,2,IQ(IP+1),19)     ! Which end hit?
          CALL MVBITS(STAT,24,2,IQ(IP+1),21)    ! Saturation, overlap
C
C ****  Now transfer drift distances and zee from charge division
C
          DRIFT_PLUS = Q(POINT+1)
          DRIFT_MINUS = Q(POINT+2)
          ZEE = Q(POINT + 3)
          AREA = Q(POINT+6)
          HITINFO(WORD1) = NINT(DRIFT_PLUS * 2000.)  ! Units of 5 microns
          HITINFO(WORD2) = NINT(DRIFT_MINUS * 2000.) ! Units of 5 microns
          HITINFO(WORD1+2) = NINT(ZEE * 100.)        ! Units of .1 mm
          HITINFO(WORD2+2) = NINT(AREA)      ! No scale factor for now
          IQ(IP+2) = HITDATA(1)
          IQ(IP+3) = HITDATA(2)
          IP = IP + NWORDS
          POINT = POINT + IQ(LVSEC+3)
        ENDDO                           ! Loop over hits
      ENDDO                             ! Loop over wires
C
C ****  Set bit in VSEC bank to indicate compression is done for this sector
C
      LVSEC = GZVSEC(LAYER,SECTOR)
      IQ(LVSEC) = IBSET(IQ(LVSEC),COMPRESS)
C
      RETURN
C
      ENTRY VHITST(LABEL)
C
      LVHIT = GZVHIT()
      IF ( LVHIT .LE. 0 ) GO TO 999
      DO 100 IWIR = 0, 23
        IF ( LABEL(IWIR) .EQ. 0 ) GO TO 100
        SEC = IBITS(LABEL(IWIR),11,5)
        LAY = IBITS(LABEL(IWIR),16,2)
        NHIT = COUNTR(SEC,LAY)
        IP = LVHIT + POINTR(SEC,LAY)
        DO IHIT = 1, NHIT
          HLABEL = IBITS(IQ(IP+1),1,17)
          IF ( LABEL(IWIR)/2 .EQ. HLABEL ) THEN
C       bit 0: side, bit 23: on VTXT
            IHSIDE = IAND(LABEL(IWIR),HTSIDE)
            BITPTN = ONVTXT + IHSIDE
            IQ(IP+1) = IOR(IQ(IP+1), BITPTN)
            GO TO 100
          ENDIF
          IP = IP + IQ(LVHIT+3)
        ENDDO
  100 CONTINUE
C
      RETURN
C
      ENTRY VHITPT(LAYER, SECTOR, PNT, NUMHIT)
C
      CALL EVNTID(RUN,ID)
      IF (RUN .NE. RUNSAV .OR. ID .NE. IDSAV .OR. NEW_EVENT) THEN
        NEW_EVENT = .FALSE.
C
C  in case of that DHITFL is not called in this program (e.g. running on
C  a STA input file), rebuild the pointor map if there is a DHIT bank exist
C
        RUNSAV = RUN
        IDSAV = ID
        CALL VZERO(POINTR(0,0),3*32)
        CALL VZERO(COUNTR(0,0),3*32)
        CALL PATHRS
        LVHIT = GZVHIT()
        IF (LVHIT .LE. 0) GOTO 300
        NHIT = IQ(LVHIT+2)
        IP = LVHIT + 3
        NLAY = -1
        NSEC = -1
        DO 301 KHIT = 1, NHIT
          KLAY = IBITS(IQ(IP+1),16,2)
          KSEC = IBITS(IQ(IP+1),11,5)
          IF (KLAY .NE. NLAY .OR. KSEC .NE. NSEC) THEN
            NLAY = KLAY
            NSEC = KSEC
            POINTR(KSEC,KLAY) = IP - LVHIT
          ENDIF
          COUNTR(KSEC,KLAY) = COUNTR(KSEC,KLAY) + 1
          IP = IP + IQ(LVHIT+3)
  301   CONTINUE
      ENDIF
  300 PNT = POINTR(SECTOR,LAYER)
      NUMHIT = COUNTR(SECTOR,LAYER)
C
      RETURN
C
      ENTRY VHIT_DROP
C
      LVHIT = GZVHIT()
      IF ( LVHIT .GT. 0 ) CALL MZDROP(IXCOM,LVHIT,' ')
C
      CALL VZERO(POINTR(0,0),3*32)
      CALL VZERO(COUNTR(0,0),3*32)
C
  999 RETURN
      END
