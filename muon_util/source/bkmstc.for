      INTEGER FUNCTION BKMSTC ( CHOPT, MODULE )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : routine to book a bank MSTC hanging from the 
C-                         MSTH under SMUO under TREE in ZEBSTP
C-
C-   Returned value  :     Address of the booked bank in ZEBSTP
C-   Inputs  :  CHOPT   =  'STPC', 'STPO', or 'STPN' reg. constants 
C-                         'PKPC', 'PKPO', or 'PKPN' packed L2 constants
C-                         MODULE = module for which the bank is booked
C-   Outputs : 
C-   Controls: 
C-
C-   Created  21-JUL-1993   J.Green
C-   Modified 10 Nov 1994   J. Wilcox and D. Wood
C-                          allow booking of "packed" banks
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE      'D0$INC:ZEBSTP.INC'
      INCLUDE      'D0$LINKS:IZSTPC.LINK'
      INCLUDE      'D0$LINKS:IZSTPN.LINK'
      INCLUDE      'D0$LINKS:IZSTPO.LINK'
      INCLUDE      'D0$LINKS:IZSMUO.LINK'
      INTEGER       MODULE
      CHARACTER*(*) CHOPT
      CHARACTER*4   TREE
      INTEGER       NHEAD               ! number of header words in bank
      INTEGER       KSUP                ! address of the supporting bank
      INTEGER       KMSTC               ! address of the created bank
      CHARACTER*60  MSGSTR              ! error message
      LOGICAL       LFIRST              ! first call
      INTEGER       IXIO                ! index to MSTC format
      INTEGER       NPLNS               ! number of planes in MODULE
      INTEGER       NWIRS               ! number of wires  in MODULE
      INTEGER       MODX                ! module # returned by MUMDAT
      INTEGER       NDATA               ! number of data words in MSTC
      INTEGER       GZMCON              ! function to get bank address
C----------------------------------------------------------------------
      DATA          LFIRST   /.TRUE./
      DATA          NHEAD    /16/
C----------------------------------------------------------------------
      BKMSTC = 0                        ! assume failure
      IF ( LFIRST ) THEN                ! need to describe the bank\
        CALL MZFORM ( 'MSTC', '12I -F', IXIO )
        LFIRST = .FALSE.
      ENDIF
C
C GET THE STP BANK
C
      IF (CHOPT(3:4).EQ.'PO') THEN
        TREE = 'STPO'
      ELSEIF (CHOPT(3:4).EQ.'PC') THEN
        TREE = 'STPC'
      ELSEIF (CHOPT(3:4).EQ.'PN') THEN
        TREE = 'STPN'
      ENDIF
C
      KSUP = GZMCON ( MODULE, 'MSTH', TREE )    ! find supporting bank
C
      IF ( KSUP .NE. 0 ) THEN           ! SMUO exists, go ahead
        IF(CHOPT(1:2).EQ.'PK') THEN
          NDATA = NHEAD + 16 * 2
        ELSE
          NDATA = NHEAD + 16 *4
        ENDIF
        CALL MZBOOK ( IDVSTP, KMSTC, KSUP, -MODULE, 'MSTC', 1,
     &      1, NDATA, 9, 0)
        BKMSTC = KMSTC
      ELSE                              ! MSTC does not exist
        MSGSTR = ' BKMSTC: Bank MSTH does not exist under '//TREE
        CALL INTMSG (MSGSTR)
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
