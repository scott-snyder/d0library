      SUBROUTINE EZDELETE (PARAM,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Delete the specified parameter from the
C-   currently selected RCP bank.
C-
C-   Inputs  : PARAM    [C*]    Name of parameter
C-   Outputs : IER      [I]     0 --- OK
C-   Controls:
C-
C-   Created  18-DEC-1990   Harrison B. Prosper
C-   Updated   4-MAR-1991   Harrison B. Prosper  
C-      Bug fix 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) PARAM
      INTEGER IER
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:LKSRCP.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
      INTEGER IPTR,PTR,I,J,L,N,NWORDS,WRDIDS,PTRVAL,IP,IPVAL,TOTAL
      INTEGER NIDS,NVAL,IPTI,IPTO,IPTV,IPTT,LOLD,LNEW
      LOGICAL EZZSHFT,EZZAND
C----------------------------------------------------------------------
C
C ****  Notes:
C ****
C ****  The structure of an RCP bank is as follows:
C ****
C ****          HEADER
C ****          IDENTIFIERS     NIDS*WRDIDS full-words
C ****          ORDER-MAP       NIDS full-words
C ****          VALUES          NVAL full-words
C ****          TYPES           NVAL full-words
C ****
C ****  The order-map is really two (independent) lists: pointers into
C ****  the VALUES block in the high-order words and an order-map in
C ****  the low-order words. These lists MUST be shifted around
C ****  independently. To simplify the pointer machinations the shifting
C ****  of blocks and pointers is done in several simple steps.
C
C ****  Clear error flag
C
      ERRSRC = EZS_SUCCESS
C
      IF ( ISRCP .LE. 0 ) THEN
        ERRSRC = EZS_BANK_NOTSELECTED
        GOTO 999
      ENDIF
C
C ****  Get pointer to parameter
C
      CALL EZGETI(PARAM(1:LEN(PARAM)),PTR,IER)
      IF ( IER .NE. EZS_SUCCESS ) THEN
        GOTO 999
      ENDIF
C
C ****  Get base pointers to data within RCP bank
C
      CALL EZZGPT (LSRCP,IPTI,IPTO,IPTV,IPTT)
C
C ****  Get pointer to values, get parameter size
C
      PTRVAL= EZZSHFT(IC(IPTO+PTR),-NBITS)      ! Pointer to 1st value
      TOTAL = EZZAND (IC(IPTT+PTRVAL),MASK)     ! Zero upper word
C
      WRDIDS= IC(LSRCP+JJNWRD)                  ! Number of fullwords/identifier
      NIDS  = IC(LSRCP+JJIDS)                   ! # of IDS
      NVAL  = IC(LSRCP+JJVAL)                   ! # of VALUES
C
C ****  Step 1  COMPRESS order-map without disturbing
C ****  the value pointers
C
      IPTR = 0
      DO I = 1, NIDS
C
C ****  Find the "I" for which the ordering pointer IP
C ****  is equal to the pointer PTR corresponding to the
C ****  parameter to be deleted; then compress from that
C ****  point on.
C
        IP = EZZAND (IC(IPTO+I),MASK)           ! Pointer to identifiers
        IF ( IP .EQ. PTR ) THEN
          IPTR = I
          GOTO 100
        ENDIF
      ENDDO
  100 CONTINUE
C
C ****  We MUST compress if 0 < IPTR < NIDS; if IPTR = NIDS then
C ****  we are deleting the last parameter in which case there
C ****  is no need to compress.
C
      IF ( (IPTR .GT. 0) .AND. (IPTR .LT. NIDS) ) THEN
        DO I = IPTR, NIDS-1
          IP = EZZAND (IC(IPTO+I+1),MASK)         ! Ptr to NEXT identifier
          IPVAL = EZZSHFT(IC(IPTO+I),-NBITS)      ! Pointer to values
          IC(IPTO+I) = EZZSHFT(IPVAL,NBITS)       ! Update pointer to values
          IC(IPTO+I) = IC(IPTO+I) + IP            ! Update ordering pointer
        ENDDO
      ENDIF
C
C ****  Step 2  COMPRESS blocks
C
C ****  Skip this section if this is the last parameter
C
      IF ( PTR .LT. NIDS ) THEN
C
C ****  Compress Identifiers
C
        NWORDS = WRDIDS*(NIDS-PTR)
        LNEW   = IPTI + WRDIDS*(PTR-1)
        LOLD   = LNEW + WRDIDS
        DO I = 1, NWORDS
          IC(LNEW+I) = IC(LOLD+I)
        ENDDO
C
C ****  Compress Value-pointers (in Order Map section) without
C ****  disturbing the Order-Map
C
        NWORDS = NIDS - PTR
        LNEW   = IPTO + PTR - 1
        LOLD   = LNEW + 1
        DO I = 1, NWORDS
          IP = EZZAND (IC(LNEW+I),MASK)           ! Ptr to identifier
          IPVAL = EZZSHFT(IC(LOLD+I),-NBITS)      ! Pointer to values
          IC(LNEW+I) = EZZSHFT(IPVAL,NBITS)       ! Update pointer to values
          IC(LNEW+I) = IC(LNEW+I) + IP            ! Update ordering pointer
        ENDDO
C
C ****  Compress Values
C
        NWORDS = NVAL - PTRVAL - TOTAL + 1
        LNEW   = IPTV + PTRVAL - 1
        LOLD   = LNEW + TOTAL
        DO I = 1, NWORDS
          IC(LNEW+I) = IC(LOLD+I)
        ENDDO
C
C ****  Compress Types
C
        NWORDS = NVAL - PTRVAL - TOTAL + 1
        LNEW   = IPTT + PTRVAL - 1
        LOLD   = LNEW + TOTAL
        DO I = 1, NWORDS
          IC(LNEW+I) = IC(LOLD+I)
        ENDDO
      ENDIF
C
C ****  Update number of identifiers and values
C
      NIDS = NIDS - 1
      NVAL = NVAL - TOTAL
      IC(LSRCP+JJIDS) = NIDS
      IC(LSRCP+JJVAL) = NVAL
C
C ****  Update values and order map pointers
C
      DO I = 1, NIDS
C
C ****  Adjust pointer to values
C
        IPVAL = EZZSHFT(IC(IPTO+I),-NBITS)      ! Old pointer to values
        IF ( IPVAL .GT. PTRVAL ) THEN
          IPVAL = IPVAL - TOTAL
        ENDIF
C
C ****  Adjust order map pointer
C
        IP = EZZAND (IC(IPTO+I),MASK)           ! Pointer to identifiers
        IF ( IP .GT. PTR ) THEN
          IP = IP - 1
        ENDIF
        IC(IPTO+I) = EZZSHFT(IPVAL,NBITS)       ! Update pointer to values
        IC(IPTO+I) = IC(IPTO+I) + IP            ! Update ordering pointer
      ENDDO
C
  999 RETURN
      END
