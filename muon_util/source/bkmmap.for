      INTEGER FUNCTION BKMMAP ( TREE, MODULE, NDATA )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : routine to book a bank MMAP hanging from the 
C-                         MMAH under SMUO under TREE in ZEBSTP
C-
C-   Returned value  :     Address of the booked bank in ZEBSTP
C-   Inputs  :             TREE   =  'STPC', 'STPO', or 'STPN'
C-                         MODULE = module for which the bank is booked
C-                         NDATA  = size of the bank
C-   Outputs : 
C-   Controls: 
C-
C-   Created  21-JUN-1989   J.Green
C-               JUN-90     J.Green     fixed call to MZFORM
C_               JUL-90     J.Green     fixed link argument in call to MZBOOK
C-            02-JUN-1992   A.Taketani  fixed format inconsitency and link
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE      'D0$INC:ZEBSTP.INC'
      INCLUDE      'D0$LINKS:IZMMAP.LINK'
      INCLUDE      'D0$LINKS:IZMMAH.LINK'
      INTEGER       MODULE
      CHARACTER*(*) TREE
      INTEGER       NDATA
      INTEGER       KSUP,KSUPO          ! address of the supporting bank
      INTEGER       KMMAP               ! address of the created bank
      CHARACTER*60  MSGSTR              ! error message
      LOGICAL       LFIRST              ! first call
      INTEGER       IXIO                ! index to MMAP format
      INTEGER       NPLNS               ! number of planes in MODULE
      INTEGER       GZMCON              ! function to get bank address
C----------------------------------------------------------------------
      DATA          LFIRST   /.TRUE./
C----------------------------------------------------------------------
      BKMMAP = 0                        ! assume failure
      IF ( LFIRST ) THEN                ! need to describe the bank\
        CALL MZFORM ( 'MMAP', '2I 1F 9I 4F 1I 1F 1I -F', IXIO )
        LFIRST = .FALSE.
      ENDIF
C
      KSUP = GZMCON ( MODULE, 'MMAH', TREE )    ! find supporting bank
C
      IF ( KSUP .NE. 0 ) THEN           ! SMUO exists, go ahead
        IF (LC(KSUP-IZMMAP).EQ.0) THEN
          CALL MZBOOK ( IDVSTP, KMMAP, KSUP, -IZMMAP, 'MMAP', 2,
     &                  2, NDATA, IXIO, 0)
        ELSE
          KSUP = LC(KSUP-IZMMAP)
          KSUPO = KSUP
  100     CONTINUE
          IF (KSUP.NE.0) THEN
            KSUPO = KSUP
            KSUP = LC(KSUP)
            GOTO 100
          END IF
          CALL MZBOOK ( IDVSTP, KMMAP, KSUPO, 0, 'MMAP', 2,
     &                  2, NDATA, IXIO, 0)
        END IF
        BKMMAP = KMMAP
      ELSE                              ! MMAH does not exist
        MSGSTR = ' BKMMAP: Bank MMAH does not exist under '//TREE
        CALL INTMSG (MSGSTR)
      ENDIF
C
  999 RETURN
      END

