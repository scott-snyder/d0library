      INTEGER FUNCTION BKMDFT ( TREE, MODULE )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : routine to book a bank MDFT hanging from the 
C-                         MDFH under SMUO under TREE in ZEBSTP
C-
C-   Returned value  :     Address of the booked bank in ZEBSTP
C-   Inputs  :             TREE   =  'STPC', 'STPO', or 'STPN'
C-                         MODULE = module for which the bank is booked
C-   Outputs : 
C-   Controls: 
C-
C-   Created  9-91 D. HEDIN
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE      'D0$INC:ZEBSTP.INC'
      INCLUDE      'D0$LINKS:IZSTPC.LINK'
      INCLUDE      'D0$LINKS:IZSTPN.LINK'
      INCLUDE      'D0$LINKS:IZSTPO.LINK'
      INCLUDE      'D0$LINKS:IZSMUO.LINK'
      INTEGER       MODULE
      CHARACTER*(*) TREE
      INTEGER       KSUP                ! address of the supporting bank
      INTEGER       KMDFT               ! address of the created bank
      CHARACTER*60  MSGSTR              ! error message
      LOGICAL       LFIRST              ! first call
      INTEGER       IXIO                ! index to MDFT format
      INTEGER       NDATA               ! number of data words in MDFT
      INTEGER       GZMCON              ! function to get bank address
C----------------------------------------------------------------------
      DATA          LFIRST   /.TRUE./
C----------------------------------------------------------------------
      BKMDFT = 0                        ! assume failure
      IF ( LFIRST ) THEN                ! need to describe the bank\
        CALL MZFORM ( 'MDFT', '10I -F', IXIO )
        LFIRST = .FALSE.
      ENDIF
C
      KSUP = GZMCON ( MODULE, 'MDFH', TREE )    ! find supporting bank
C
      IF ( KSUP .NE. 0 ) THEN           ! SMUO exists, go ahead
        NDATA = 33          
        CALL MZBOOK ( IDVSTP, KMDFT, KSUP, -MODULE, 'MDFT', 1,
     &      1, NDATA, 9, 0)
        BKMDFT = KMDFT
      ELSE                              ! MDFH does not exist
        MSGSTR = ' BKMDFT: Bank MDFH does not exist under '//TREE
        CALL INTMSG (MSGSTR)
      ENDIF
C
  999 RETURN
      END
