      SUBROUTINE ENZSTP (BANK1,KSTPX,IBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return address of specified STPx bank.
C-
C-   Inputs  : BANK1       Bank name STPO, STPC, STPN
C-   Outputs : KSTPX       Bank address
C-                        -1 .... Invalid bank name
C-                         0 .... Bank does not exist
C-             IBANK       1 .... STPO
C-                         2 .... STPC
C-                         3 .... STPN
C-                        -1 .... Invalid bank name
C-   Controls: 
C-
C-   Created   7-MAR-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*4 BANK1,BANK
      INTEGER KSTPX,IBANK
C
      INCLUDE 'D0$LINKS:IZSTPO.LINK'
      INCLUDE 'D0$LINKS:IZSTPC.LINK'
      INCLUDE 'D0$LINKS:IZSTPN.LINK'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
C
      CALL UPCASE (BANK1,BANK)
      IF ( BANK .EQ. 'STPO' ) THEN
        IBANK = 1
        KSTPX = LC(LSTPH-IZSTPO)
      ELSEIF ( BANK .EQ. 'STPC' ) THEN
        IBANK = 2
        KSTPX = LC(LSTPH-IZSTPC)
      ELSEIF ( BANK .EQ. 'STPN' ) THEN
        IBANK = 3
        KSTPX = LC(LSTPH-IZSTPN)
      ELSE
        IBANK =-1
        KSTPX =-1
      ENDIF
  999 RETURN
      END
