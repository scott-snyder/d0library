      INTEGER FUNCTION BKMSRH ( TREE, MODULE )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : routine to book a bank MSRH hanging from the 
C-                         SMUO under TREE in ZEBSTP
C-
C-   Returned value  :     Address of the booked bank in ZEBSTP
C-   Inputs  :             TREE   =  'STPC', 'STPO', or 'STPN'
C-                         MODULE   dummy argument to make this routine 
C-                                  look like other muon BK routines
C-   Outputs : 
C-   Controls: 
C-
C-   Created  02-aug-1990   S.T.Repond
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE      'D0$INC:ZEBSTP.INC'
      INCLUDE      'D0$LINKS:IZSTPC.LINK'
      INCLUDE      'D0$LINKS:IZSTPN.LINK'
      INCLUDE      'D0$LINKS:IZSTPO.LINK'
      INCLUDE      'D0$LINKS:IZSMUO.LINK'
      INCLUDE      'D0$LINKS:IZMGEH.LINK'
      INCLUDE      'D0$LINKS:IZMSRH.LINK'
      INTEGER       MODULE
      CHARACTER*(*) TREE
      INTEGER       NMUON    /307/      ! number of links
      INTEGER       KSTPX               ! address of the bank TREE
      INTEGER       KSUP                ! address of the supporting bank
      INTEGER       KMSRH               ! address of the created bank
      CHARACTER*60  MSGSTR              ! error message
      LOGICAL       LFIRST  /.TRUE./    ! first call
      INTEGER       IXIO                ! index to MSRH format
C----------------------------------------------------------------------
      BKMSRH = 0                        ! assume failure
      IF (LFIRST) THEN                  ! need to describe the bank
        CALL MZFORM ( 'MSRH', '10I ', IXIO )
        LFIRST = .FALSE.
      ENDIF
C                     first find the bank TREE
      KSTPX = 0
      IF     (TREE .EQ. 'STPO') THEN
        KSTPX = LC(LSTPH-IZSTPO)
      ELSEIF (TREE .EQ. 'STPC') THEN
        KSTPX = LC(LSTPH-IZSTPC)
      ELSEIF (TREE .EQ. 'STPN') THEN
        KSTPX = LC(LSTPH-IZSTPN)
      ENDIF
C
      IF ( KSTPX .NE. 0 ) THEN          ! TREE exists, go ahead
        KSUP = LC(KSTPX-IZSMUO)
        IF ( KSUP .NE. 0 ) THEN         ! SMUO exists, go ahead
          CALL MZBOOK ( IDVSTP, KMSRH, KSUP, -IZMSRH, 'MSRH', NMUON,
     &      NMUON, 10, 9, 0)
          BKMSRH = KMSRH
        ELSE                            ! SMUO does not exist
          MSGSTR = ' BKMSRH: Bank SMUO does not exist under '//TREE
          CALL INTMSG (MSGSTR)
        ENDIF
      ELSE                              ! TREE does not exist
        MSGSTR = ' BKMSRH: Bank '//TREE//' does not exist '
        CALL INTMSG (MSGSTR)
      ENDIF
C
  999 RETURN
      END
