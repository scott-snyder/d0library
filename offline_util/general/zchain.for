C DEC/CMS REPLACEMENT HISTORY, Element ZCHAIN.FOR
C *1     7-FEB-1990 23:04:38 STEWART "DROP_BANKS ASSOCIATED ROUTINES"
C DEC/CMS REPLACEMENT HISTORY, Element ZCHAIN.FOR
      SUBROUTINE ZCHAIN
     &  (IXDIV,LSTART,LTARGT,MXLINK,BANK,IZLINK,NLINK,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : For given TARGET bank return the link offsets
C-   which connect that bank to the first bank of the zebra structure.
C-   BANK(1) and IZLINK(1) contain, respectively, the first bank and link
C-   in the chain.
C-   
C-   Starting with the first bank one can reach the given bank
C-   by tracing down the chain of STRUCTURAL links using the integer
C-   function:
C-
C-   LBANK = LCHAIN (LSTART,IZLINK,NLINK)
C-   
C-   which returns the address of the target bank.
C-
C-   Inputs  : IXDIV    [I]     UNUSED as yet
C-             LSTART   [I]     Start bank address
C-             LTARGT   [I]     Target bank address
C-             MXLINK   [I]     Maximum number of links
C-
C-   Outputs : BANK(*)  [C*]    List of names of connecting banks
C-                              including the Start bank
C-             IZLINK(*)[I]     List of link offsets
C-             NLINK    [I]     Number of link offsets
C-             IER      [I]      0 -- OK
C-                              -1 -- Incomplete chain between banks
C-                              -2 -- Invalid Start or Target bank address
C-                              -3 -- Maximum buffer size MXLINK reached
C-   Controls:
C-
C-   Created  19-JAN-1990   Harrison B. Prosper, Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER IXDIV
      INTEGER LSTART
      INTEGER LTARGT
      INTEGER MXLINK
      CHARACTER*(*) BANK(*)
      INTEGER IZLINK(*)
      INTEGER NLINK
      INTEGER IER
C
      CHARACTER*4 BB
      INTEGER I,J,II
      INTEGER LBANK,LSUPP,NS,LLINK
      LOGICAL FOUND
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
      IER   = 0
      NLINK = 0
      IF ( LSTART .LE. 0 ) THEN
        IER =-2
        GOTO 999
      ENDIF
      IF ( LTARGT .LE. 0 ) THEN
        IER =-2
        GOTO 999
      ENDIF
C
      LBANK = LTARGT
      DO WHILE ( LBANK .GT.0 )
        LSUPP = LQ(LBANK+1)             ! Address of support (UP) bank
        LLINK = LQ(LBANK+2)             ! Address of support link
C
C ****  Check if this bank is in a linear chain.
C ****  by testing if IQ(LLINK-4) = IQ(LBANK-4), that is, check
C ****  if the names are identical.
C
        IF ( IQ(LLINK-4) .EQ. IQ(LBANK-4) ) THEN
          LSUPP = LLINK
        ENDIF
C
        IF ( LSUPP .GT. 0 ) THEN
          NS = IQ(LSUPP-2)              ! Number of structural links
C
          IF ( NS .GT. 0 ) THEN
            FOUND = .FALSE.
            II =-1                      ! Include linear chains of banks
            DO WHILE ( II .LT. NS )     ! Determine link offset
              II = II + 1
              IF ( LQ(LSUPP-II) .EQ. LBANK ) THEN
                IF ( NLINK .LT. MXLINK ) THEN
                  NLINK = NLINK + 1
                  IZLINK(NLINK) = II
                  CALL DHTOC (4,IQ(LSUPP-4),BANK(NLINK))  ! Get bank name
                  II = NS                 ! Exit loop
                  FOUND = .TRUE.          ! Found link offset
                ELSE
                  IER =-3
                  GOTO 999
                ENDIF
              ENDIF
            ENDDO
            IF ( .NOT. FOUND ) THEN
              IER =-1                   ! Link not found
              GOTO 999
            ENDIF
          ELSE
            IER =-1
            GOTO 999
          ENDIF
        ELSE
          IER =-1
          GOTO 999
        ENDIF
C
C ****  Check if support bank is the START bank
C
        IF ( LSUPP .EQ. LSTART ) THEN
          LBANK = 0                     ! Exit loop
        ELSE
          LBANK = LSUPP                 ! Move to support bank
        ENDIF
      ENDDO
C
C ****  Invert order of link offsets
C
      DO I = 1,NLINK
        J = NLINK-I+1
        IF ( I .GE. J ) GOTO 999
        II = IZLINK(I)
        IZLINK(I) = IZLINK(J)
        IZLINK(J) = II
        BB = BANK(I)
        BANK(I) = BANK(J)
        BANK(J) = BB
      ENDDO
C
  999 RETURN
      END
