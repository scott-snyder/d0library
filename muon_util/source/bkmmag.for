      INTEGER FUNCTION BKMMAG ( TREE, MODULE )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : routine to book a bank MMAG hanging from the 
C-                         MMAH under SMUO under TREE in ZEBSTP
C-
C-   Returned value  :     Address of the booked bank in ZEBSTP
C-   Inputs  :             TREE   =  'STPC', 'STPO', or 'STPN'
C-                         MODULE = module for which the bank is booked
C-   Outputs : 
C-   Controls: 
C-
C-   Created  21-JUN-1989   J.Green
C-               JUN-90     J.Green     fixed call to MZFORM
C-            02-JUN-1992   A.TAKETANI  correct LINK
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE      'D0$INC:ZEBSTP.INC'
      INCLUDE      'D0$LINKS:IZSTPC.LINK'
      INCLUDE      'D0$LINKS:IZSTPN.LINK'
      INCLUDE      'D0$LINKS:IZSTPO.LINK'
      INCLUDE      'D0$LINKS:IZSMUO.LINK'
      INCLUDE      'D0$LINKS:IZMMAG.LINK'
      INCLUDE 'D0$INC:QUEST.INC'
      INTEGER       MODULE
      CHARACTER*(*) TREE
      INTEGER       KSUP,KSUPO          ! address of the supporting bank
      INTEGER       KMMAG               ! address of the created bank
      CHARACTER*60  MSGSTR              ! error message
      LOGICAL       LFIRST              ! first call
      INTEGER       IXIO                ! index to MMAG format
      INTEGER       NPLNS               ! number of planes in MODULE
      INTEGER       GZMMAH,KMMAH        ! function to get bank address
      INTEGER       LZLAST, LSUP
C----------------------------------------------------------------------
      DATA          LFIRST   /.TRUE./
C----------------------------------------------------------------------
      BKMMAG = 0                        ! assume failure
      IF ( LFIRST ) THEN                ! need to describe the bank\
        CALL MZFORM ( 'MMAG', '2I 1F 9I 1H -F', IXIO )
        LFIRST = .FALSE.
      ENDIF
C
      KMMAH = GZMMAH(0)             ! find supporting bank
C
      IF ( KMMAH .NE. 0 ) THEN           ! SMUO exists, go ahead
        IF ( LC(KMMAH-IZMMAG).EQ.0 ) THEN
          CALL MZBOOK ( IDVSTP, KMMAG, KMMAH, -IZMMAG, 'MMAG', 2,
     &      2, 29, IXIO, 0)
        ELSE
          LSUP = LZLAST(IDVSTP, KMMAH-IZMMAG )
          CALL MZBOOK ( IDVSTP, KMMAG, LSUP, 0, 'MMAG', 2, 
     &      2, 29, IXIO, 0 )
        END IF
        BKMMAG = KMMAG
      ELSE                              ! MMAH does not exist
        MSGSTR = ' BKMMAG: Bank MMAH does not exist under '//TREE
        CALL INTMSG (MSGSTR)
      ENDIF
C
  999 RETURN
      END
