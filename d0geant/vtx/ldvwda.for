      SUBROUTINE LDVWDA( VDAT, NPULSE )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Load hits from one sense wire end into ZEBRA
C-                         bank VWDA.  VWDA is booked if needed.
C-
C-   Inputs  : VDAT(IWORD,IHIT) contains 9 data words per pulse.
C-               IVDAT(1,IHIT) = logical address (see VCODER)
C-               VDAT(2,IHIT)  = drift time (ns)
C-               VDAT(3,IHIT)  = pulse area (counts)
C-               VDAT(4,IHIT)  = pulse width (ns)
C-               VDAT(5,IHIT)  = peak height (counts)
C-               VDAT(6,IHIT)  = drift time error (ns)
C-               VDAT(7,IHIT)  = pulse area error (counts)
C-               IVDAT(8,IHIT) = status
C-               IVDAT(9,IHIT) = track id (see above)
C-             NPULSE = number of pulses on channel
C-             LAYER, SECTOR, WIRE, END in VTLOCA
C-
C-   Outputs : Fills VWDA
C-   Controls: none
C-
C-   Created  12-NOV-1989   Peter Grudberg
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:GCUNIT.INC'
      INCLUDE 'D0$INC:D0LOG.INC'
      INCLUDE 'D0$INC:VTXLNK.INC'
      INCLUDE 'D0$INC:VTLOCA.INC'
C
      INTEGER NPULSE, NWDSHT, NFADC, MAXDAT, NDMAX
      PARAMETER ( NWDSHT = 9 )
      PARAMETER ( NDMAX = 50 )          ! max data / wire-end
      PARAMETER ( NFADC = 16 )          ! 8 wires * 2 ends
      PARAMETER ( MAXDAT = NFADC * NDMAX )      ! max data / sector
      INTEGER LKVWDA, LKVSEC, LBASE
      INTEGER NHTS, IPTR, IPTRHT, IPULSE, IWDHT
      REAL VDAT(NWDSHT,*)
C----------------------------------------------------------------------
C
C ****  If VSEC is not yet booked, hits are not being processed.
C ****  Book bank VSEC( LAYER, SECTOR ) with no room for hits, just to support
C ****  VWDA( LAYER, SECTOR )
C
      LKVSEC = LVSEC( SECTOR, LAYER )
      IF ( LKVSEC .LE. 0 ) CALL BKVSEC( LAYER, SECTOR, 0, LKVSEC )
C
C ****  Get link LKVWDA for sector
C
      LKVWDA = LVWDA( SECTOR, LAYER )
      IF (LKVWDA .LE. 0) CALL BKVWDA( LAYER, SECTOR, MAXDAT, LKVWDA)
      IPTR = 4 + 2 * WIRE + END
C
C ****  Check if data is already present
C
      NHTS = IQ( LKVWDA + IPTR )
      IF ( NHTS .NE. 0 ) THEN
        WRITE (LOUT,*) ' **** LDVWDA: error; attempting overwrite'
        GO TO 999
      ENDIF
C
C ****  Load # pulses and pointer to first pulse on channel
C
      IQ( LKVWDA + IPTR ) = NPULSE
      IPTRHT = 4 + NFADC*2 + NWDSHT*IQ(LKVWDA+1)
      IQ( LKVWDA + IPTR + NFADC ) = IPTRHT
C
C ****  Increment # pulses in sector
C
      IQ( LKVWDA + 1 ) = IQ( LKVWDA + 1 ) + NPULSE
C
C ****  Transfer hits to ZEBRA bank VWDA
C
      DO IPULSE = 1, NPULSE
        LBASE = LKVWDA + IPTRHT - 1 + NWDSHT*(IPULSE-1)
        DO IWDHT = 1, NWDSHT
          Q(LBASE+IWDHT) = VDAT(IWDHT,IPULSE)
        ENDDO
      ENDDO
C
  999 RETURN
      END
