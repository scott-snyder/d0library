C+
      INTEGER FUNCTION BKSMTH ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : makes a header ZEBRA bank for SAMUS
C-                         minimum time banks.
C-
C-   Returned value  : SAMUS minimum times header bank address.
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
C-   Updated  08-May-1993   Evgeny Kozlovsky  MZFORM was called
C-                                            to corect format
C- 
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSMTH.LINK'
      INTEGER N_STATION, N_SECTION
      PARAMETER (N_STATION=6, N_SECTION=6)
      INTEGER KSSAM, KSMTH, NSTA, NSEC
      INTEGER NWORDS, NLINKS
      INTEGER GZSSAM, GZSMTH
      INTEGER NFORM
C----------------------------------------------------------------------
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
C
      BKSMTH = 0
C
C     check existance of the banks
C
      KSSAM = GZSSAM()                  ! SAMUS header bank address
      IF (KSSAM .EQ. 0) CALL BKSSAM ()
C
C     creation SAMUS minimum times header bank
C
      KSMTH = GZSMTH ()                 ! SAMUS geom header bank address
      IF (KSMTH .EQ. 0) THEN            ! bank does not exists
C
        IF(FIRST) THEN
          CALL MZFORM('SMTH','11I ', NFORM )
          FIRST = .FALSE.
        ENDIF
C
        NLINKS = N_STATION * N_SECTION
        NWORDS = 11
        CALL MZBOOK (IDVSTP, KSMTH, KSSAM, -IZSMTH, 'SMTH',
     &               NLINKS, NLINKS, NWORDS, NFORM , 0 )
      END IF
      BKSMTH = KSMTH
C
C     creation SAMUS station minimum time banks
C
      DO NSTA = 1, N_STATION
        DO NSEC = 1, N_SECTION
          CALL BKSMNT (NSTA, NSEC)
        END DO
      END DO
C                              
      RETURN
      END
