      INTEGER FUNCTION BKSMNT (NSTA, NSEC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : makes a ZEBRA banks for SAMUS minimum times
C-
C-   Returned value  : SAMUS minimum times header bank address
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  20-SEP-1990   V. Glebov & A. Efimov & V. Podstavkov
C-   Updated  28-SEP-1990   V. Glebov & A. Efimov & V. Podstavkov   
C-                          Change original link
C-   Updated  30-SEP-1990   V. Glebov & A. Efimov & V. Podstavkov  
C-                          Add error messages 
C-   Updated  13-DEC-1992   Alexander Efimov  Change the number of
C-                          words in SMNT banks.
C-   Updated  15-DEC-1992   Alexander Efimov  Change the tree of the
C-                          ZEBRA banks.
C-   Updated   4-JAN-1993   Daria Zieminska  remove the call to BKSMTH
C-                          (BKSMTH calls BKSMNT) 
C-   Updated  12-Feb-1993   Alexander Kozelov  Insert GZSMNT call
C-                          to avoid ZEBSTP overflow
C-   Updated  08-May-1993   Evgeny Kozlovsky  MZFORM was called
C-                                            to corect format
C-   Note: max. value power of expansion for dist is N=24 
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSMTH.LINK'
      INTEGER NSTA, NSEC
      INTEGER GZSSAM, GZSMTH, GZSMNT
      INTEGER KSSAM, KSMTH
      INTEGER IL, KSMNT, NWORDS, NLINKS
      INTEGER NFORM
C----------------------------------------------------------------------
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      INTEGER N_STATION, N_SECTION
      PARAMETER (N_STATION=6, N_SECTION=6)
      INTEGER N_ADLT(N_SECTION)
      DATA N_ADLT /4, 6, 4, 4, 6, 4/
C
      BKSMNT = 0
C
C     check existance of the banks
C
      KSSAM = GZSSAM()                  ! SAMUS header bank address
      IF (KSSAM .EQ. 0) CALL BKSSAM ()
C
C     creation SAMUS minimum times header bank
C
      KSMTH = GZSMTH ()                 ! SAMUS geom header bank address
      IF (KSMTH .EQ. 0) THEN
        GO TO 999
      END IF 
C
C     creation SAMUS station minimum time banks
C
      KSMNT = GZSMNT( NSTA, NSEC )      ! SAMUS min. time bank address
      IF (KSMNT.EQ.0) THEN              ! bank does not exists
c
        IF(FIRST) THEN
          CALL MZFORM( 'SMNT', '12I / 1I 24F', NFORM )
          FIRST = .FALSE.
        ENDIF
C
        NLINKS = 0
        NWORDS = 10 + 2 + 25 * N_ADLT(NSEC)
        IL = (NSTA - 1) * N_SECTION + NSEC
        CALL MZBOOK (IDVSTP, KSMNT, KSMTH, -IL, 'SMNT',
     &               NLINKS, NLINKS, NWORDS, NFORM , 0 )
      ENDIF
      BKSMNT = KSMTH
C                              
 999  RETURN
      END
