      SUBROUTINE L0_DECODE_HEADER(LL0DN,VERSION,NWORDS,DETAILS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Decode Crate Header block.
C-
C-   Inputs  : LL0DN = Address of the Zeroth word of the Crate requested.
C-             i.e. LL0DN + 1 = Pointer to the first word = Header length.
C-             VERSION = Data Version Number = 1 only.
C-   Outputs : NWORDS = Number of data words decoded, including header word.
C-             DETAILS = Data word information as explained below.
C-   Controls: none
C-
C-   Created   2-JUN-1992   Jeffrey Bantly  modified from CD_DECODE_HEADER
C-   Updated  30-NOV-1992   Jeffrey Bantly  correctly set NWORDS 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER LL0DN
      INTEGER NWORDS,HLENG,SYNCW,CRTWR,VERSN,BUNCH
      INTEGER DETAILS(5,7),I,J,VERSION
C
C----------------------------------------------------------------------
C
      VERSION = 1
      DO 10 I = 1,5
        DO 20 J = 1,7
          DETAILS(I,J) = 0              ! initialize output array
   20   CONTINUE
   10 CONTINUE
      NWORDS = 0
C
      IF (LL0DN.EQ.0) GO TO 999
C
C  *** Decode Header word :
C
      IF (VERSION.EQ.0) GO TO 999
      HLENG = IQ(LL0DN + 1)             ! Word-1
      NWORDS = HLENG + 1                ! Total Number of words returned.
      DETAILS(1,1) = HLENG              ! Header length
C
C   *** Decode Sync word :
C
      SYNCW = IQ(LL0DN + 2)             ! Word-2
      DETAILS(2,1) = IBITS(SYNCW,16,16) ! Beam crossing number
C
C   *** Decode Controller word
C
      CRTWR = IQ(LL0DN + 3)             ! Word-3
      DETAILS(3,1) = IBITS(CRTWR,24,8)  ! Crate ID
      DETAILS(3,2) = IBITS(CRTWR,16,8)  ! # of Level 0 data blocks
      DETAILS(3,3) = IBITS(CRTWR,0,16)  ! Crate Mode (1=data,2=calib,3=laser)
C
C   *** Decode Version Number
C
      VERSN = IQ(LL0DN + 4)             ! Word-4
C
      DETAILS(4,1) = IBITS(VERSN,24,8)  ! System word
      DETAILS(4,2) = IBITS(VERSN,16,8)  ! User word
      DETAILS(4,3) = IBITS(VERSN,0,16)  ! Version number
C
C   *** Decode Bunch Identifier word
C
      BUNCH = IQ(LL0DN + 5)             ! Word-5
C
      DETAILS(5,1) = IBITS(BUNCH,13,1)  ! Bunch 5, valid=1
      DETAILS(5,2) = IBITS(BUNCH,12,1)  ! Bunch 4, valid=1
      DETAILS(5,3) = IBITS(BUNCH,11,1)  ! Bunch 3, valid=1
      DETAILS(5,4) = IBITS(BUNCH,10,1)  ! Bunch 2, valid=1
      DETAILS(5,5) = IBITS(BUNCH, 9,1)  ! Bunch 1, valid=1
      DETAILS(5,6) = IBITS(BUNCH, 8,1)  ! Bunch 0, valid=1
      DETAILS(5,7) = IBITS(BUNCH, 0,8)  ! Level 0 Bunch ID of present crossing
C
C   *** Done.
C
      NWORDS = 5
C---------------------------------------------------------------------------
  999 RETURN
      END
