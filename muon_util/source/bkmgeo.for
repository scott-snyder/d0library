      INTEGER FUNCTION BKMGEO ( TREE, MODULE )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : routine to book a bank MGEO hanging from the 
C-                         MGEH under SMUO under TREE in ZEBSTP
C-
C-   Returned value  :     Address of the booked bank in ZEBSTP
C-   Inputs  :             TREE   =  'STPC', 'STPO', or 'STPN'
C-                         MODULE = module for which the bank is booked
C-   Outputs : 
C-   Controls: 
C-
C-   Created  21-JUN-1989   J.Green
C-               JUN-90     J.Green     fixed call to MZFORM
C-   Updated  13-MAY-1992   A.Taketani  fixed MZFORM format for new MGEO
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
      INTEGER       KMGEO               ! address of the created bank
      CHARACTER*60  MSGSTR              ! error message
      LOGICAL       LFIRST              ! first call
      INTEGER       IXIO                ! index to MGEO format
      INTEGER       GZMCON              ! function to get bank address
C----------------------------------------------------------------------
      DATA          LFIRST   /.TRUE./
C----------------------------------------------------------------------
      BKMGEO = 0                        ! assume failure
      IF ( LFIRST ) THEN                ! need to describe the bank\
        CALL MZFORM ( 'MGEO', '2I 1F 10I -F', IXIO )
        LFIRST = .FALSE.
      ENDIF
C
      KSUP = GZMCON ( MODULE, 'MGEH', TREE )    ! find supporting bank
C
      IF ( KSUP .NE. 0 ) THEN           ! SMUO exists, go ahead
        CALL MZBOOK ( IDVSTP, KMGEO, KSUP, -MODULE, 'MGEO', 2,
     &      2, 58, IXIO, 0)
        BKMGEO = KMGEO
      ELSE                              ! MGEH does not exist
        MSGSTR = ' BKMGEO: Bank MGEH does not exist under '//TREE
        CALL INTMSG (MSGSTR)
      ENDIF
C
  999 RETURN
      END
