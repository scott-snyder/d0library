      INTEGER FUNCTION BKMGAN ( CHOPT, MODULE )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : routine to book a bank MGAN hanging from the 
C-                         MGNH under SMUO under TREE in ZEBSTP
C-
C-   Returned value  :     Address of the booked bank in ZEBSTP
C-   Inputs  : CHOPT   =  'STPC', 'STPO', or 'STPN' reg. constants
C-                        'PKPC','PKPO', or 'PKPN' packed L2 constants
C-                         MODULE = module for which the bank is booked
C-   Outputs : 
C-   Controls: 
C-
C-   Created  21-JUN-1989   J.Green
C-   DH 8/92 FIX FOR ODD WIRE CHAMBERS (AND NON-24)
C-   Updated  23-JAN-1993 j.balderston FOR PACKED L2 DATA(RE BKMTIM)
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
      INTEGER       KMGAN               ! address of the created bank
      CHARACTER*60  MSGSTR              ! error message
      LOGICAL       LFIRST              ! first call
      INTEGER       IXIO                ! index to MGAN format
      INTEGER       NPLNS,NWIRS,MODX    ! number of planes in MODULE
      INTEGER       NDATA               ! number of data words in MGAN
      INTEGER       GZMCON              ! function to get bank address
C----------------------------------------------------------------------
      DATA          LFIRST   /.TRUE./
C----------------------------------------------------------------------
      BKMGAN = 0                        ! assume failure
      IF ( LFIRST ) THEN                ! need to describe the bank\
        CALL MZFORM ( 'MGAN', '12I -F', IXIO )
        LFIRST = .FALSE.
      ENDIF
C
C  GET THE STP BANK
C
      IF(CHOPT(3:4).EQ.'PO') THEN
        TREE= 'STPO'
      ELSEIF(CHOPT(3:4).EQ.'PC') THEN
        TREE= 'STPC'
      ELSEIF(CHOPT(3:4).EQ.'PN') THEN
        TREE='STPN'
      ENDIF
C
      KSUP = GZMCON ( MODULE, 'MGNH', TREE )    ! find supporting bank
C
      IF ( KSUP .NE. 0 ) THEN           ! SMUO exists, go ahead
        CALL MUMDAT ( MODULE, MODX, NPLNS, NWIRS )
        IF(MOD(NWIRS,2).EQ.1) NWIRS=NWIRS+1   ! correct for odd-wire
                                        ! readout
        IF(CHOPT(1:2).EQ.'PK') THEN
          NDATA=16 + 4*NPLNS*NWIRS
        ELSE
          NDATA = 16 + 8*NPLNS*NWIRS          ! #wires * 2 chnls * 4
                                        ! words
        ENDIF
        CALL MZBOOK ( IDVSTP, KMGAN, KSUP, -MODULE, 'MGAN', 1,
     &      1, NDATA, 9, 0)
        BKMGAN = KMGAN
      ELSE                              ! MGNH does not exist
        MSGSTR = ' BKMGAN: Bank MGNH does not exist under '//TREE
        CALL INTMSG (MSGSTR)
      ENDIF
C
  999 RETURN
      END
