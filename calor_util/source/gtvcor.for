      SUBROUTINE GTVCOR( TYPE, IVCOR, IVERSION, DP, VARDP, IER )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return information about a particular VCOR bank
C-
C-   Inputs  :        TYPE    [C*4]   Character flag describing which
C-                                    VCOR desired. 'ALL ' = all
C-                    IVCOR     [I]   Which of these VCOR banks to return
C-                                    information for.
C-   Outputs :
C-                  IVERSION    [I]   Version number
C-                    DP(5)     [R]   Corrected - Uncorrected values
C-                                    for Ex,Ey,Ez,E and Scalar ET
C-                    VARDP(5)  [R]   Variances on these quantities
C-                    IER       [I]   Error code
C-                                    0 = ok
C-                                    -1= No such VCOR bank found
C-
C-   Controls:
C-
C-  ENTRY GTVCOR_TOTAL( TYPE, NVCOR, IER )
C-
C-   Returns the number of VCOR banks of this TYPE. 'ALL ' returns total
C-    number of VCOR banks
C-
C-  ENTRY GTVCOR_POINTER( LVCOR )
C-  
C-   Return zebra pointer to the last VCOR looked at
C-   in a GTVCOR call
C-
C-   Created  26-OCT-1993   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      REAL DP(5), VARDP(5)
      INTEGER IVCOR, IER, IVERSION, NVCOR
      CHARACTER*4 TYPE 
C
      CHARACTER*4 TYPE_FOUND
      INTEGER LVCOR, GZVCOR, ICOUNT, I
      INTEGER LVCOR_RET
      SAVE LVCOR
C----------------------------------------------------------------------

C
C: Status default is OK
C
      IER = 0
C
C: Find the VCOR they want
C
      IF ( IVCOR .LE. 0 ) THEN
        IER = -1
        GOTO 999
      ENDIF
C
      ICOUNT = 0
      LVCOR  = GZVCOR()
      DO WHILE ( LVCOR .GT. 0 .AND. ICOUNT .LT. IVCOR )
        CALL UHTOC( IQ(LVCOR+2), 4, TYPE_FOUND, 4 )
        IF ( (TYPE_FOUND .EQ. TYPE) .OR. ( TYPE .EQ. 'ALL ') ) ICOUNT =
     &    ICOUNT + 1
        IF ( ICOUNT .LT. IVCOR ) LVCOR  = LQ( LVCOR )
      ENDDO
C
      IF ( LVCOR .LE. 0 ) THEN
        IER = -1
        GOTO 999
      ENDIF
C
C: Fill output variables
C
      IVERSION = IQ( LVCOR + 1 )
      DO I = 1, 4
        DP( I )    = Q( LVCOR + 2 + I )
        VARDP( I ) = Q( LVCOR + 6 + I )
      ENDDO

      DP( 5 )      = Q( LVCOR + 11 )
      VARDP( 5 )   = Q( LVCOR + 12 )

  999 RETURN
C*************************************************************
C: ENTRY to return last vcor looked at by GTVCOR
C*************************************************************
      ENTRY GTVCOR_POINTER( LVCOR_RET )
      LVCOR_RET = LVCOR
      RETURN
C****************************************************************
C: ENTRY point to return number of VCOR of this type
C****************************************************************

      ENTRY GTVCOR_TOTAL( TYPE, NVCOR, IER )
      IER   = 0
      NVCOR = 0
C
C: Loop over all VCOR banks and count number of this type
C
      LVCOR  = GZVCOR()
      DO WHILE ( LVCOR .GT. 0 )
        CALL UHTOC( IQ(LVCOR+2), 4, TYPE_FOUND, 4 )
        IF ( (TYPE_FOUND .EQ. TYPE) .OR. ( TYPE .EQ. 'ALL ') ) NVCOR =
     &    NVCOR + 1
        LVCOR  = LQ( LVCOR )
      ENDDO
      END
