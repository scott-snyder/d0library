      INTEGER FUNCTION BKMBAD ( TREE, MODULE )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : routine to book a bank MBAD hanging from the 
C-                         MGNH under SMUO under TREE in ZEBSTP
C-
C-   Returned value  :     Address of the booked bank in ZEBSTP
C-   Inputs  :             TREE   =  'STPC', 'STPO', or 'STPN'
C-                         MODULE = module for which the bank is booked
C-   Outputs : 
C-   Controls: 
C-
C-   Created  21-JUN-1989   J.Green
C            JUL-90 C.Francis - put in the call to MUMDAT 
C            FEB-90 J.Green   - incresed to 3 words per cell   
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
      INTEGER       KMBAD               ! address of the created bank
      CHARACTER*60  MSGSTR              ! error message
      INTEGER       NMODX
      INTEGER       NWIRX               ! number of wires in module
      INTEGER       NPLNX               ! number of planes in module
      INTEGER       NDATA               ! number of data words in MBAD
      INTEGER       GZMCON              ! function to get bank address
C----------------------------------------------------------------------
      BKMBAD = 0                        ! assume failure
C
      KSUP = GZMCON ( MODULE, 'MBHD', TREE )    ! find supporting bank
C
      IF ( KSUP .NE. 0 ) THEN           ! SMUO exists, go ahead
        CALL MUMDAT(MODULE,NMODX,NPLNX,NWIRX)
        NDATA = 14 + 36*NPLNX          ! 12 READOUT CELLS * 3 chnls 
        CALL MZBOOK ( IDVSTP, KMBAD, KSUP, -MODULE, 'MBAD', 1,
     &      1, NDATA, 2, 0)
        BKMBAD = KMBAD
      ELSE                              ! MBHD does not exist
        MSGSTR = ' BKMBAD: Bank MBHD does not exist under '//TREE
        CALL INTMSG (MSGSTR)
      ENDIF
C
  999 RETURN
      END
