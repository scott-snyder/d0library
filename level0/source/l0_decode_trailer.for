      SUBROUTINE L0_DECODE_TRAILER(LL0DN,VERSION,NWORDS,DETAILS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Decode Crate Trailer block.
C-
C-   Inputs  : LL0DN = Address of the last word before the first trailer 
C-                     word of Crate 01
C-                     i.e. LL0DN + 1 = Trailer word 1
C-   Outputs : VERSION = Version number of trailer type
C-             NWORDS = Number of data words decoded into DETAILS
C-             DETAILS = Data word information as explained below.
C-   Controls: none
C-
C-   Created   2-JUN-1992   Jeffrey Bantly 
C-   Updated  30-NOV-1992   Jeffrey Bantly  correctly set NWORDS 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER LL0DN
      INTEGER NWORDS
      INTEGER TOT_WORD_CNT
      INTEGER TRGCRTW
      INTEGER CRATE_ID
      INTEGER TKNPW
      INTEGER CHKSW
      INTEGER DETAILS(4,4)
      INTEGER I,J
      INTEGER VERSION
C
C----------------------------------------------------------------------
C
      VERSION = 1
      DO 10 I = 1,4
        DO 20 J = 1,4
          DETAILS(I,J) = 0              ! initialize output array
   20   CONTINUE
   10 CONTINUE
      NWORDS = 0
C
      IF (LL0DN.LE.0) GO TO 999
C
      IF (VERSION.EQ.0) GO TO 999
C
C   *** Decode the Total Word Count word :
C
      TOT_WORD_CNT = IQ(LL0DN + 1)              ! Word-1
      DETAILS(1,1) = TOT_WORD_CNT               ! Trailer length
C
C   *** Decode Trig ID / Crate ID word :
C
      TRGCRTW = IQ(LL0DN + 2)                   ! Word-2
      DETAILS(2,1) = IBITS(TRGCRTW,16,16)       ! Trigger Number
      CRATE_ID = IBITS(TRGCRTW, 0,16)           ! Crate ID
      DETAILS(2,2) = CRATE_ID                   ! Crate ID
      DETAILS(2,3) = INT(CRATE_ID/10)           ! Crate Number
      DETAILS(2,4) = CRATE_ID - 10*DETAILS(2,2) ! Bank Number
C
C   *** Decode Token Pass Status word :
C
      TKNPW = IQ(LL0DN + 3)                     ! Word-3
      DETAILS(3,1) = TKNPW                      ! Token Pass Word
C
C   *** Decode Checksum word :
C
      CHKSW = IQ(LL0DN + 4)                     ! Word-4
      DETAILS(4,1) = CHKSW                      ! Total Checksum for the crate
C
C   *** Done.
C
      NWORDS = 4
C-------------------------------------------------------------------------
  999 RETURN
      END
