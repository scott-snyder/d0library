      INTEGER FUNCTION BKSELH ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create the Zebra banks SELH 
C-                         ( Samus ELectronic Header bank ) 
C-
C-   Returned value  : bank SELH address (or zero if something is bad)
C-
C-   Inputs  : None
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
C-   Updated  14-DEC-1992   Alexander Efimov: add creation SELC banks.
C-   Updated  07-MAY-1993   S. Abachi  NIO was chaged from 1 to 2 for integer
C-                          bank.
C-   Updated  08-May-1993   Evgeny Kozlovsky  MZFORM was called
C-                                            to corect format
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSELH.LINK'
      INTEGER N_STATION, N_SECTION
      PARAMETER (N_STATION=6, N_SECTION=6)
      INTEGER KSSAM, KSELH  
      INTEGER NWORDS, NLINKS, NSTA, NSEC
      INTEGER GZSSAM, GZSELH, BKSSAM
      INTEGER NIO
C----------------------------------------------------------------------
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      BKSELH = 0
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
C
C ****  Create bank SELH
C
      KSELH = GZSELH ()                 ! electronic header bank address
      IF (KSELH .EQ. 0) THEN            ! bank does not exist
c..
        IF(FIRST) THEN
          CALL MZFORM( 'SELH','  -I ', NIO )
          FIRST = .FALSE.
        ENDIF
c..
        NLINKS = 36 
        NWORDS = 10
        CALL MZBOOK (IDVSTP, KSELH, KSSAM, -IZSELH, 'SELH', 
     &               NLINKS, NLINKS, NWORDS, NIO , 0 )
      END IF
C
C ****  create SELC banks
C
      DO NSTA = 1, N_STATION
        DO NSEC = 1, N_SECTION
          CALL BKSELC (NSTA, NSEC)
        END DO
      END DO
      BKSELH = KSELH
C                              
      RETURN
      END
