      INTEGER FUNCTION BKSMUO ( TREE, MODULE )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : routine to book a bank SMUO hanging from the
C-                         bank TREE in ZEBSTP
C-
C-   Returned value  :     Address of the booked bank in ZEBSTP
C-   Inputs  :             TREE   =  'STPC', 'STPO', or 'STPN'
C-                         MODULE   dummy argument to make this routine 
C-                                  look like other muon BK routines
C-
C-   Created  21-JUN-1989   J.Green
C-   Updated  15-MAY-1992   A.Taketani, Bank format correction
C-   Updated  Jul-93        J.Green, add 2 more links (MSTH + spare)
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
      INTEGER       KSMUO               ! address of the created bank
      CHARACTER*60  MSGSTR              ! error message
      INTEGER       IXIO
      LOGICAL       FIRST
      DATA          FIRST/.TRUE./
C----------------------------------------------------------------------
C               decide bank format
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL MZFORM( 'SMUO', '-I', IXIO )
      END IF
C
C               first find the supporting bank
      KSUP = 0
      IF     (TREE .EQ. 'STPO') THEN
        KSUP = LC(LSTPH-IZSTPO)
      ELSEIF (TREE .EQ. 'STPC') THEN
        KSUP = LC(LSTPH-IZSTPC)
      ELSEIF (TREE .EQ. 'STPN') THEN
        KSUP = LC(LSTPH-IZSTPN)
      ENDIF
C
      IF ( KSUP .NE. 0 ) THEN
        CALL MZBOOK ( IDVSTP, KSMUO, KSUP, -IZSMUO, 'SMUO', 12, 12, 8,
     &               IXIO, 0)
        BKSMUO = KSMUO
      ELSE
        MSGSTR = ' BKSMUO: Supporting bank '//TREE//' does not exist '
        CALL INTMSG (MSGSTR)
        BKSMUO = 0
      ENDIF
C
  999 RETURN
      END
