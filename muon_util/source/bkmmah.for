      INTEGER FUNCTION BKMMAH ( TREE, MODULE )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : routine to book a bank MMAH hanging from the 
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
C-   Updated  02-Jun-1992   A.Taketani Fixed format and link number
C-   Updated  13-APR-1993   S.Igarashi For magnet rotation ver.4
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE      'D0$INC:ZEBSTP.INC'
      INCLUDE      'D0$LINKS:IZSTPC.LINK'
      INCLUDE      'D0$LINKS:IZSTPN.LINK'
      INCLUDE      'D0$LINKS:IZSTPO.LINK'
      INCLUDE      'D0$LINKS:IZSMUO.LINK'
      INCLUDE      'D0$LINKS:IZMMAH.LINK'
      INTEGER       MODULE
      CHARACTER*(*) TREE
      INTEGER       KSTPX               ! address of the bank TREE
      INTEGER       KSUP                ! address of the supporting bank
      INTEGER       KMMAH               ! address of the created bank
      CHARACTER*60  MSGSTR              ! error message
      INTEGER       IXIO
      LOGICAL       FIRST
      DATA          FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL MZFORM( 'MMAH', '2I 1F 7I -F', IXIO )
      END IF
C
      BKMMAH = 0                        ! assume failure
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
          CALL MZBOOK ( IDVSTP, KMMAH, KSUP, -IZMMAH, 'MMAH', 3,
     &      3, 46, IXIO, 0)
          BKMMAH = KMMAH
        ELSE                            ! SMUO does not exist
          MSGSTR = ' BKMMAH: Bank SMUO does not exist under '//TREE
          CALL INTMSG (MSGSTR)
        ENDIF
      ELSE                              ! TREE does not exist
        MSGSTR = ' BKMMAH: Bank '//TREE//' does not exist '
        CALL INTMSG (MSGSTR)
      ENDIF
C
  999 RETURN
      END
