      INTEGER FUNCTION BKMPED ( CHOPT, MODULE )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : routine to book a bank MPED hanging from the 
C-                         MPDH under SMUO under TREE in ZEBSTP
C-
C-   Returned value  :     Address of the booked bank in ZEBSTP
C-   Inputs  :  CHOPT   =  'STPC', 'STPO', or 'STPN' reg. constants 
C-                         'PKPC', 'PKPO', or 'PKPN' packed L2 constants
C-                         MODULE = module for which the bank is booked
C-   Outputs : 
C-   Controls: 
C-
C-   Created  21-JUN-1989   J.Green
C-   Updated  12-JUN-1991   R.Zazula  change TREE to CHOPT for selecting
C-                          between regular and Level 2 data. No test
C-                            for invalid CHOPTS
C-    DH 8/92 FIX FOR ODD WIRED CHAMBERS 
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE      'D0$INC:ZEBSTP.INC'
      INCLUDE      'D0$LINKS:IZSTPC.LINK'
      INCLUDE      'D0$LINKS:IZSTPN.LINK'
      INCLUDE      'D0$LINKS:IZSTPO.LINK'
      INCLUDE      'D0$LINKS:IZSMUO.LINK'
      INTEGER       MODULE
      CHARACTER*(*) CHOPT
      CHARACTER*4   TREE
      INTEGER       KSUP                ! address of the supporting bank
      INTEGER       KMPED               ! address of the created bank
      CHARACTER*60  MSGSTR              ! error message
      LOGICAL       LFIRST              ! first call
      INTEGER       IXIO                ! index to MPED format
      INTEGER       NPLNS               ! number of planes in MODULE
      INTEGER       NWIRS               ! number of wires  in MODULE
      INTEGER       MODX                ! module # returned by MUMDAT
      INTEGER       NDATA               ! number of data words in MPED
      INTEGER       GZMCON              ! function to get bank address
C----------------------------------------------------------------------
      DATA          LFIRST   /.TRUE./
C----------------------------------------------------------------------
      BKMPED = 0                        ! assume failure
      IF ( LFIRST ) THEN                ! need to describe the bank\
        CALL MZFORM ( 'MPED', '12I -F', IXIO )
        LFIRST = .FALSE.
      ENDIF
C
C GET THE STP BANK
C
      IF (CHOPT(3:4).EQ.'PO') THEN
        TREE = 'STPO'
      ELSEIF (CHOPT(3:4).EQ.'PC') THEN
        TREE = 'STPC'
      ELSEIF (CHOPT(3:4).EQ.'PN') THEN
        TREE = 'STPN'
      ENDIF
C
      KSUP = GZMCON ( MODULE, 'MPDH', TREE )    ! find supporting bank
C
      IF ( KSUP .NE. 0 ) THEN           ! SMUO exists, go ahead
        CALL MUMDAT ( MODULE, MODX, NPLNS, NWIRS )
        IF(MOD(NWIRS,2).EQ.1) NWIRS=NWIRS+1   ! correct for odd-wire readout
        IF(CHOPT(1:2).EQ.'PK') THEN
          NDATA = 16 + 2*(NPLNS)*(NWIRS) ! PACKED L2 DATA
        ELSE
          NDATA = 16 + 8*NWIRS*NPLNS    ! planes*wires*4chanls*2words
        ENDIF
        CALL MZBOOK ( IDVSTP, KMPED, KSUP, -MODULE, 'MPED', 1,
     &      1, NDATA, 9, 0)
        BKMPED = KMPED
      ELSE                              ! MPED does not exist
        MSGSTR = ' BKMPED: Bank MPDH does not exist under '//TREE
        CALL INTMSG (MSGSTR)
      ENDIF
C
  999 RETURN
      END
