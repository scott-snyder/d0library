      SUBROUTINE CD_DECODE_HEADER(LCDDN,VERSION,NWORDS,DETAILS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Decode Crate Header block.
C-
C-   Inputs  : LCDDN = Address of the Zeroth word of the Crate requested.
C-             i.e. LCDDN + 1 = Pointer to the first word = Header length.
C-             VERSION = Data Version Number = 0,1,2 or 3.
C-             Versions 3 and above have the same Header Structure.
C-   Outputs : NWORDS = Number of data words decoded
C-             DETAILS = Data word information as explained in Docs.
C-   Controls: none
C-
C-   Created  25-MAR-1990   Srini Rajagopalan
C-   Updated  01-AUG-1990   SR, Accomodate all data formats.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER LCDDN,NWORDS,HLENG,SYNCW,CRTWR,VERSN
      INTEGER DETAILS(5,5),I,J,VERSION
C
C----------------------------------------------------------------------
C
      DO 10 I = 1,5
        DO 20 J = 1,5
          DETAILS(I,J) = 0              ! initialize output array
   20   CONTINUE
   10 CONTINUE
      NWORDS = 0
C
      IF (LCDDN.EQ.0) GO TO 999
C
      IF (VERSION.EQ.0) GO TO 999
      HLENG = IQ(LCDDN + 1)             ! Word-1
      NWORDS = HLENG + 1                ! Total Number of words retruned.
      DETAILS(1,1) = HLENG              ! Header length
C
C   *** Decode Sync word :
C
      SYNCW = IQ(LCDDN + 2)             ! Word-2
      DETAILS(2,1) = IBITS(SYNCW,16,16) ! Beam crossing number
C
      IF (VERSION.EQ.1) GO TO 999
C
C   *** Decode Controller word
C
      CRTWR = IQ(LCDDN + 3)             ! Word-3
      IF (VERSION .EQ. 2) THEN
        DETAILS(3,1) = IBITS(CRTWR,16,16) ! Crate ID
        DETAILS(3,2) = IBITS(CRTWR,12,4)  ! # of FADC cards
        DETAILS(3,3) = IBITS(CRTWR,8,4)   ! Crate Mode (undefined)
        DETAILS(3,4) = IBITS(CRTWR,6,2)   ! Data type
      ELSE
        DETAILS(3,1) = IBITS(CRTWR,24,8)  ! Crate ID
        DETAILS(3,2) = IBITS(CRTWR,16,8)  ! # of FADC cards
        DETAILS(3,3) = IBITS(CRTWR,8,8)   ! Crate Mode (undefined)
        DETAILS(3,4) = IBITS(CRTWR,0,8)   ! Data type
      ENDIF
C
C   *** Decode Version Number
C
      IF (VERSION.EQ.2) THEN
        VERSN = IQ(LCDDN + 5)             ! Word-5
      ELSE IF (VERSION.GT.2) THEN
        VERSN = IQ(LCDDN + 4)             ! Word-4 
      ENDIF
C
      DETAILS(4,1) = IBITS(VERSN,24,8)  ! System word
      DETAILS(4,2) = IBITS(VERSN,16,8)  ! User word
      DETAILS(4,3) = IBITS(VERSN,0,16)  ! Version number
C
  999 RETURN
      END
