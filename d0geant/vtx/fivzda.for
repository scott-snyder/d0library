      SUBROUTINE FIVZDA( IADDR, POINT, NHITZ )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Process the pulses from one z-strip channel.
C-                         Data is in the common VZDATA.
C-
C-   Inputs  : IADDR [I] = channel address
C-             POINT [I] = pointer to the location in the index array of the
C-                         first pulse on the channel to be processed.
C-             NHITZ [I] = number of pulses on this channel
C-   Outputs : none
C-   Controls: called by BLVZDA
C-
C-   Created  17-NOV-1989   Peter Grudberg
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCUNIT.INC'
      INCLUDE 'D0$INC:VZDATA.INC'
C
      INTEGER POINT, NHITZ
      INTEGER NWDSDA, MAXDAT
      PARAMETER ( NWDSDA = 9 )
      PARAMETER ( MAXDAT = 50 )
      INTEGER IVDAT(NWDSDA,MAXDAT), IORD, INEXT, JOUT, IADDR
      REAL VDAT(NWDSDA,MAXDAT), THRESH
      EQUIVALENCE ( VDAT, IVDAT )
      DATA THRESH / 0.0 /
C----------------------------------------------------------------------
      IF ( NHITZ .LE. 0 ) GO TO 999
C
      JOUT = 0
      DO 100 INEXT = POINT, POINT + NHITZ - 1
        IORD = ZDAT_INDX(INEXT)
C
C ****  If the pulse is below threshold, ignore it
C
        IF ( ZDAT_PHGT(IORD) .LT. THRESH ) GO TO 100
C
C ****  Store the pulse in ZDAT:
C
        IF ( JOUT .GE. MAXDAT ) THEN
          WRITE (LOUT,*)
     &    ' **** FIVZDA: Too many hits on one channel, data lost'
          GO TO 101
        ELSE
          JOUT = JOUT + 1
          IVDAT(1,JOUT) = IADDR           ! channel address
          VDAT(2,JOUT)  = ZDAT_TIME(IORD) ! drift time (ns)
          VDAT(3,JOUT)  = ZDAT_PHGT(IORD) ! pulse area
          VDAT(4,JOUT)  = ZDAT_PWID(IORD) ! pulse width (ns)
          VDAT(5,JOUT)  = ZDAT_PHGT(IORD) ! peak, same as area for now
          VDAT(6,JOUT)  = 7.              ! drift time error (ns)
          VDAT(7,JOUT)  = 11.             ! pulse area error
          IVDAT(8,JOUT) = 0               ! status word
          IVDAT(9,JOUT) = ZDAT_TRAK(IORD) ! track id
        ENDIF
  100 CONTINUE                          ! loop over hits
  101 CONTINUE                          ! overflow exit
C
C ****  Load pulses into VZDA:
C
      IF ( JOUT .LE. 0 ) GO TO 999
      CALL LDVZDA( IADDR, VDAT, JOUT )
C
  999 RETURN
      END
