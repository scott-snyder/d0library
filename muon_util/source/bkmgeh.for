      INTEGER FUNCTION BKMGEH ( TREE, MODULE )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : routine to book a bank MGEH hanging from the 
C-                         SMUO under TREE in ZEBSTP
C-
C-   Returned value  :     Address of the booked bank in ZEBSTP
C-   Inputs  :             TREE   =  'STPC', 'STPO', or 'STPN'
C-                         MODULE   dummy argument to make this routine 
C-                                  look like other muon BK routines
C-   Outputs : 
C-   Controls: 
C-
C-   Created  21-JUN-1989   J.Green
C-   Updated  21-MAY-1992   A.Taketani
C-   Updated  28-DEC-1993   A.T. Expand data word size to 24
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
      INTEGER       MODULE
      CHARACTER*(*) TREE
      INTEGER       NMUON               ! number of links
      INTEGER       KSTPX               ! address of the bank TREE
      INTEGER       KSUP                ! address of the supporting bank
      INTEGER       KMGEH               ! address of the created bank
      CHARACTER*60  MSGSTR              ! error message
      LOGICAL       LFIRST              ! first call
      INTEGER       IXIO                ! index to MGEH format
C----------------------------------------------------------------------
      DATA          LFIRST   /.TRUE./
      DATA          NMUON    /307/ 
C----------------------------------------------------------------------
      BKMGEH = 0                        ! assume failure
      IF (LFIRST) THEN                  ! need to describe the bank
        CALL MZFORM ( 'MGEH', '2I 1F 5I -F', IXIO )
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
          CALL MZBOOK ( IDVSTP, KMGEH, KSUP, -IZMGEH, 'MGEH', NMUON,
     &      NMUON, 24, IXIO, 0)
          BKMGEH = KMGEH
        ELSE                            ! SMUO does not exist
          MSGSTR = ' BKMGEH: Bank SMUO does not exist under '//TREE
          CALL INTMSG (MSGSTR)
        ENDIF
      ELSE                              ! TREE does not exist
        MSGSTR = ' BKMGEH: Bank '//TREE//' does not exist '
        CALL INTMSG (MSGSTR)
      ENDIF
C
  999 RETURN
      END
