      SUBROUTINE LDVZDA( IADDR, VDAT, NPULSE )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Load hits from one strip channel into ZEBRA
C-                         bank VZDA.  VZDA is booked if needed.
C-
C-   Inputs  : IADDR [I] = packed channel address
C-             VDAT(IWORD,IHIT) contains 9 data words per pulse.
C-               IVDAT(1,IHIT) = logical address (see VCODER)
C-               VDAT(2,IHIT)  = drift time (ns)
C-               VDAT(3,IHIT)  = pulse area (counts)
C-               VDAT(4,IHIT)  = pulse width (ns)
C-               VDAT(5,IHIT)  = peak height (counts)
C-               VDAT(6,IHIT)  = drift time error (ns)
C-               VDAT(7,IHIT)  = pulse area error (counts)
C-               IVDAT(8,IHIT) = status
C-               IVDAT(9,IHIT) = track id
C-             NPULSE = number of pulses on channel
C-
C-   Outputs : Fills VZDA
C-   Controls: called by FIVZDA
C-
C-   Created  17-NOV-1989   Peter Grudberg
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:GCUNIT.INC'
      INCLUDE 'D0$INC:D0LOG.INC'
      INCLUDE 'D0$INC:VTXLNK.INC'
C
      INTEGER IADDR, NPULSE, NWDSHT, NFADC, MAXDAT
      PARAMETER ( NWDSHT = 9 )
      PARAMETER ( MAXDAT = 8000 )       ! 50 hits/sec * 5 strips/hit *
                                        ! 32 sectors
      INTEGER LKVZDA, LKVZLA, LBASE
      INTEGER NHTS, IPTR, IPTRHT, IPULSE, IWDHT
      INTEGER TYPE, IZLAY, ISCT, IWIR, ISTRIP, IEND, UBIT
      REAL VDAT(NWDSHT,*)
C----------------------------------------------------------------------
C
C ****  Decode Logical address
C
      CALL VCODER(IADDR,TYPE,IZLAY,ISCT,IWIR,ISTRIP,IEND,UBIT,1)
C
C ****  If VZLA is not yet booked, hits are not being processed.
C ****  Book bank VZLA( IZLAY ) with no room for hits, just to support VZDA.
C
      LKVZLA = LVZLA( IZLAY )
      IF ( LKVZLA .LE. 0 ) CALL BKVZLA( IZLAY, 0, LKVZLA )
C
C ****  Get link LKVZDA for IZLAY
C
      LKVZDA = LVZDA( IZLAY )
      IF (LKVZDA .LE. 0) CALL BKVZDA( IZLAY, MAXDAT, LKVZDA )
      NFADC = IQ( LKVZDA + 2 )
      IF ( IZLAY .EQ. 2 ) THEN
        IPTR = 4 + 2 * ISTRIP + IEND
      ELSE
        IPTR = 4 + ISTRIP
      ENDIF
C
C ****  Check if data is already present
C
      NHTS = IQ( LKVZDA + IPTR )
      IF ( NHTS .NE. 0 ) THEN
        WRITE (LOUT,*) ' **** LDVZDA: error; attempting overwrite'
        GO TO 999
      ENDIF
C
C ****  Load # pulses and pointer to first pulse on channel
C
      IQ( LKVZDA + IPTR ) = NPULSE
      IPTRHT = 4 + NFADC*2 + NWDSHT*IQ(LKVZDA+1)
      IQ( LKVZDA + IPTR + NFADC ) = IPTRHT
C
C ****  Increment # pulses in layer
C
      IQ( LKVZDA + 1 ) = IQ( LKVZDA + 1 ) + NPULSE
C
C ****  Transfer hits to ZEBRA bank VZDA
C
      DO IPULSE = 1, NPULSE
        LBASE = LKVZDA + IPTRHT - 1 + NWDSHT*(IPULSE-1)
        DO IWDHT = 1, NWDSHT
          Q(LBASE+IWDHT) = VDAT(IWDHT,IPULSE)
        ENDDO
      ENDDO
C
  999 RETURN
      END
