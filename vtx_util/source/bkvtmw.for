      SUBROUTINE BKVTMW(LAYER, IVERS, LVTMW)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book VTMW bank.  For monte carlo data (IVERS=0),
C-                         this bank holds t0 and drift velocity for each wire.
C-                         For real data, this bank holds t0s for both ends of
C-                         the wire, sigmas of the t0s, and a scaling factor
C-                         used for time-to-distance conversion.
C-
C-   Inputs  : LAYER - usual, starting from 0
C-             IVERS - bank version number
C-   Outputs : LVTMW - address of VTMW bank
C-   Controls: 
C-
C-   Created  22-JUN-1990   ED OLTMAN
C-   Updated  18-OCT-1990   ED OLTMAN  Increase number of links for additional
C-                                     sectors; add additional words for
C-                                     pointing to appropriate DTM bank
C-   Updated  31-MAR-1991   Peter Grudberg  add version number argument 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZVTMW.LINK'
c I/O:
      INTEGER LVTMW, IVERS, LAYER
c Locals:
      INTEGER ND, NFORM0, NFORM1, NFRMMC, NFORM
      INTEGER ITEMS, NSEC, NWIRE, LRUN, HRUN, NL
      LOGICAL FIRST
c Locals:
      INTEGER ISETVN
c Data:
      DATA NWIRE / 8 /
      DATA LRUN, HRUN / 0, 9999 /
      DATA FIRST / .TRUE. /
C----------------------------------------------------------------------
      IF ( LVTMH .EQ. 0 ) THEN
        CALL ERRMSG('VTMH = 0', 'BKVTMW',
     &    'Supporting bank does not exist','W')
        GO TO 999
      ENDIF
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL MZFORM('VTMW', '5I -F', NFRMMC)
        CALL MZFORM('VTMW', '5I  640F 16I', NFORM0)
        CALL MZFORM('VTMW', '5I 1280F 32I', NFORM1)
      ENDIF
      IF ( IVERS .EQ. 1 ) THEN
        ITEMS = 5                       ! Words per wire
      ELSE
        ITEMS = 2
      ENDIF
      IF ( LAYER .EQ. 0 ) THEN
        NSEC = 16
        IF ( IVERS .EQ. 1 ) THEN
          NFORM = NFORM0
        ELSE
          NFORM = NFRMMC
        ENDIF
      ELSE
        NSEC = 32
        IF ( IVERS .EQ. 1 ) THEN
          NFORM = NFORM1
        ELSE
          NFORM = NFRMMC
        ENDIF
      ENDIF
      ND = NSEC * NWIRE * ITEMS + 5     ! number of data words
      NL = 0                            ! number of struc links
      IF ( IVERS .EQ. 1 ) THEN
        ND = ND + NSEC
        NL = NSEC
      ENDIF
      CALL MZBOOK(IDVSTP, LVTMW, LVTMH, -(IZVTMW+LAYER), 'VTMW',
     &            NL, NL, ND, NFORM, 0)
      IC(LVTMW-5) = LAYER
      IC(LVTMW  ) = ISETVN(IC(LVTMW), IVERS)
      IC(LVTMW+1) = LRUN
      IC(LVTMW+2) = HRUN
      IC(LVTMW+3) = ITEMS
      IC(LVTMW+4) = NWIRE
      IC(LVTMW+5) = NSEC
  999 RETURN
      END
