      INTEGER FUNCTION BKSELC (NSTA, NSEC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create the Zebra bank SELC
C-                         ( Samus ELectronic Constants bank )
C-
C-   Returned value  : bank SELC address (or zero if something is bad)
C-
C-   Inputs  : NSTA - Station number
C-             NSEC - Section number
C-   Outputs : None
C-   Controls: None
C-
C-   Created  20-SEP-1990   V. Glebov & A. Efimov & V. Podstavkov
C-   Updated  28-SEP-1990   V. Glebov & A. Efimov & V. Podstavkov
C-                          Change the original link
C-   Updated  30-SEP-1990   V. Glebov & A. Efimov & V. Podstavkov
C-                          Add error messages
C-   Updated  30-JUN-1992   Vladimir Glebov: Change structure to
C-                          SAMCON_TREE.ZEB Version 2.0
C-   Updated  24-JUL-1992   Oleg Eroshin : Fix bags.
C-   Updated  15-DEC-1992   Alexander Efimov  : add BKSSAM and BKSELH
C-                          calls.
C-   Updated   4-JAN-1993   Daria Zieminska   remove call to BKSELH
C-                          (BKSELH calls BKSELC)
C-   Updated   7-May-1993   S. ABACHI   MZFORM was called to corect format
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSELH.LINK'
      INTEGER NSTA, NSEC
      INTEGER N_STATION, N_SECTION
      PARAMETER (N_STATION=6, N_SECTION=6)
      INTEGER BKSSAM, BKSELH
      INTEGER KSSAM, KSELH, KSELC
      INTEGER NFORM, IL, NWORDS, NLINKS
      INTEGER GZSSAM, GZSELC
      INTEGER N_CHAN_SEC(N_SECTION)
      LOGICAL FIRST
      DATA FIRST /.TRUE./
      DATA N_CHAN_SEC /128, 192, 128, 128, 192, 128/
CC
      BKSELC = 0
C
C ****  Check existence of the SSAM and SELH banks
C
      KSSAM = GZSSAM ()                 ! SAMUS header bank address
      IF (KSSAM .EQ. 0) THEN            ! bank does not exist
        KSSAM = BKSSAM ()               ! create SAMUS header bank
        IF (KSSAM .EQ. 0) THEN          ! bank does not exist
          CALL INTMSG (' BKSELH: Supporting bank SSAM does not exist')
          RETURN
        END IF
      END IF
      KSELH = LC(KSSAM-IZSELH)          ! electronic header bank address
      IF (KSELH .EQ. 0) THEN            ! bank does not exist
        CALL INTMSG (' BKSELH: Bank SELH does not exist')
        RETURN
      END IF
C
C ****  Create SELC banks
C
C
      IF(FIRST) THEN
        CALL MZFORM('SELC','10I / 6F',NFORM)
        FIRST = .FALSE.
      ENDIF
      KSELC = GZSELC (NSTA, NSEC)
      IF (KSELC .EQ. 0) THEN
        NLINKS = 0
        NWORDS = 10 + 6 * N_CHAN_SEC(NSEC)
        IL = (NSTA - 1) * N_SECTION + NSEC
        CALL MZBOOK (IDVSTP, KSELC, KSELH, -IL, 'SELC',
     &               NLINKS, NLINKS, NWORDS, NFORM, 0)
      END IF
      BKSELC = KSELC
C
      RETURN
      END
