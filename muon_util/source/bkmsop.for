      INTEGER FUNCTION BKMSOP ( TREE, MODULE,IERBK )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : routine to book a bank MSOP hanging from the
C-                         MGEH under SMUO under TREE in ZEBSTP
C-
C-   Returned value  :     Address of the booked bank in ZEBSTP
C-   Inputs  :             TREE   =  'STPC', 'STPO', or 'STPN'
C-                         MODULE = module for which the bank is booked
C-   Outputs :
C-   Controls:    IERBK    set (=1) if could not book the bank
C-
C-   Created  27-JUN-1990   S.T.Repond
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
      INTEGER       KMSOP               ! address of the created bank
      CHARACTER*60  MSGSTR              ! error message
      LOGICAL       LFIRST   /.TRUE./   ! first call
      INTEGER       IXIO                ! index to MSOP format
      INTEGER       NPLNS               ! number of planes in MODULE
      INTEGER       GZMCON              ! function to get bank address
      INTEGER GZMSRH
      INTEGER       IERBK               ! returns error if bank has not
C                                         been booked
C----------------------------------------------------------------------
      BKMSOP = 0                        ! assume failure
      IERBK = 1
C
      IF ( LFIRST ) THEN                ! need to describe the bank\
        CALL MZFORM ( 'MSOP', '16I -F', IXIO )
        LFIRST = .FALSE.
      ENDIF
C
      KSUP = GZMSRH (0) ! find supporting bank
C
      IF ( KSUP .NE. 0 ) THEN           ! SMUO exists, go ahead
        CALL MZBOOK ( IDVSTP, KMSOP, KSUP, -MODULE, 'MSOP', 2,
     &      2, 33, 9, 0)
        IERBK = 0
        BKMSOP = KMSOP
        MSGSTR = ' BKMSOP: Bank MSRH has been booked  '
        CALL INTMSG (MSGSTR)
      ELSE                              ! MGEH does not exist
        MSGSTR = ' BKMSOP: Bank MSRH does not exist under '//TREE
        CALL INTMSG (MSGSTR)
      ENDIF
C
  999 RETURN
      END
