C DEC/CMS REPLACEMENT HISTORY, Element LCTREE.FOR
C *1     7-FEB-1990 23:03:27 STEWART "DROP_BANKS ASSOCIATED ROUTINES"
C DEC/CMS REPLACEMENT HISTORY, Element LCTREE.FOR
      INTEGER FUNCTION LCTREE (IXDIV,BANK,LSTART)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Search a ZEBRA TREE of banks, starting at
C-   the bank with address LSTART, for the bank with name given in BANK
C-   and, if found, return its address.
C-
C-   Returned value  :  LCTREE > 0      Address of target bank (BANK)
C-                      LCTREE = 0      Bank NOT found
C-                      LCTREE =-1      LSTART is zero or negative
C-                      LCTREE =-2      Internal buffer overflow
C-
C-   Inputs  : IXDIV    [I]     Division number (Unused as yet)
C-             BANK     [C*]    Bank whose address is sought
C-             LSTART   [I]     Address of bank at which to
C-                              start tree scan.
C-
C-   Outputs : None
C-   Controls: None
C-
C-   Created  27-JAN-1990   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER IXDIV
      CHARACTER*(*) BANK
      INTEGER LSTART
C
      INTEGER II,JJ,KK,LL,JBASE,KBASE,NS,LBANK,IBANK,NSUP
C
      INTEGER OFFSET
      PARAMETER( OFFSET = 4095 )        ! Maximum number of banks/depth
      INTEGER LSUPP(2*OFFSET)

      INTEGER I,LDEBUG,LEVEL
      CHARACTER*4 BNK(OFFSET)
      LOGICAL DEBUG
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      DATA LDEBUG/1/
      DATA DEBUG /.FALSE./
C----------------------------------------------------------------------
      LCTREE = 0
C
      IF ( LSTART .LE. 0 ) THEN
        LCTREE =-1
        GOTO 999
      ENDIF
C
      CALL DCTOH (4,BANK(1:4),IBANK)    ! Convert bank name to INTEGER
C
C ****  Initialize search
C
      JBASE = 0
      KBASE = OFFSET
      LSUPP(JBASE+1) = LSTART
C
C ****  Start scanning loop
C
      IF ( DEBUG ) THEN
        WRITE(LDEBUG,90)
   90   FORMAT(1X,/,' <<< LCTREE-called >>>')
        LEVEL = 0
      ENDIF
C
      NSUP  = 1                         ! Only one bank at this level
      DO WHILE ( NSUP .GT. 0 )
C
        IF ( DEBUG ) THEN
          DO I =  1,NSUP
            CALL DHTOC (4,IQ(LSUPP(JBASE+I)-4),BNK(I))
          ENDDO
          LEVEL = LEVEL + 1
          WRITE(LDEBUG,95) LEVEL
   95     FORMAT(' Level  - ',I3)
          WRITE(LDEBUG,100) (BNK(I),I=1,NSUP)
  100     FORMAT(16(1X,A4))
        ENDIF
C
        KK = 0                          ! Number of banks at next level down
        DO JJ = 1, NSUP                 ! LOOP over support banks
          LL = LSUPP(JBASE+JJ)          ! Get support bank address
          NS = IQ(LL-2)                 ! Get number of structural links
C
          IF ( NS .GT. 0 ) THEN
C
            DO II = 1,NS                ! LOOP over structural links
              LBANK = LQ(LL-II)         ! Bank address hanging from link
              IF ( LBANK .GT. 0 ) THEN
                IF ( IQ(LBANK-4) .EQ. IBANK ) THEN      ! Compare names
                  LCTREE = LBANK                        ! Bank FOUND
                  GOTO 999
                ELSE
                  IF ( KK .LT. OFFSET ) THEN    ! Check buffer quota
                    KK = KK + 1                 ! Bank NOT FOUND
                    LSUPP(KBASE+KK) = LBANK     ! Note bank address
                  ELSE
                    LCTREE =-2                  ! Buffer full
                    GOTO 999
                  ENDIF
                ENDIF
              ENDIF
            ENDDO
          ENDIF
        ENDDO
C
C ****  Setup for to search next level in tree
C
        NSUP = KK
        IF ( JBASE .GT. 0 ) THEN
          JBASE = 0
          KBASE = OFFSET
        ELSE
          JBASE = OFFSET
          KBASE = 0
        ENDIF
      ENDDO
C
  999 RETURN
      END
