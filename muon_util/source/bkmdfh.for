      INTEGER FUNCTION BKMDFH ( TREE, MODULE )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : routine to book a bank MDFH hanging from the 
C-                         SMUO under TREE in ZEBSTP
C-
C-   Returned value  :     Address of the booked bank in ZEBSTP
C-   Inputs  :             TREE   =  'STPC', 'STPO', or 'STPN'
C-                         MODULE   dummy argument to make this routine 
C-                                  look like other muon BK routines
C-   Outputs : 
C-   Controls: 
C-
C-   Created  9-91   D. HEDIN
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE      'D0$INC:ZEBSTP.INC'
      INCLUDE      'D0$LINKS:IZSTPC.LINK'
      INCLUDE      'D0$LINKS:IZSTPN.LINK'
      INCLUDE      'D0$LINKS:IZSTPO.LINK'
      INCLUDE      'D0$LINKS:IZSMUO.LINK'
      INCLUDE      'D0$LINKS:IZMDFH.LINK'
      INTEGER       MODULE
      CHARACTER*(*) TREE
      INTEGER       NMUON               ! number of links
      INTEGER       KSTPX               ! address of the bank TREE
      INTEGER       KSUP                ! address of the supporting bank
      INTEGER       KMDFH               ! address of the created bank
      CHARACTER*60  MSGSTR              ! error message
C----------------------------------------------------------------------
      DATA          NMUON    /307/
C----------------------------------------------------------------------
      BKMDFH = 0                        ! assume failure
C                first find the bank TREE
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
          CALL MZBOOK ( IDVSTP, KMDFH, KSUP, -IZMDFH, 'MDFH', NMUON,
     &      NMUON, 10, 2, 0)
          BKMDFH = KMDFH
        ELSE                            ! SMUO does not exist
          MSGSTR = ' BKMDFH: Bank SMUO does not exist under '//TREE
          CALL INTMSG (MSGSTR)
        ENDIF
      ELSE                              ! TREE does not exist
        MSGSTR = ' BKMDFH: Bank '//TREE//' does not exist '
        CALL INTMSG (MSGSTR)
      ENDIF
C
  999 RETURN
      END
