      SUBROUTINE EZEND
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-      Complete construction of SRCP bank. This routine MUST be called
C-      after filling an SRCP bank with EZFILL. Note:
C-      If an SRCP bank has not been selected the routine simply returns.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  29-SEP-1988   Harrison B. Prosper
C-   Updated  16-NOV-1988   Harrison B. Prosper
C-                          MAJOR CHANGE: Uses NEW SRCP bank format
C-   Updated  11-MAY-1990   Harrison B. Prosper
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER      LIDS,LPTI,LPTO,IPTO,IPTI,MASK1
      INTEGER      I,J,K,II,JJ,NN,WRDIDS,CHRIDS
      INTEGER EZZAND,EZZSHFT
      CHARACTER*32 IDENTF
C
      INCLUDE 'D0$LINKS:IZSRCP.LINK'
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:LKSRCP.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      INCLUDE 'D0$INC:BFSRCP.INC'
      INCLUDE 'D0$INC:BFSRCP1.INC'
C----------------------------------------------------------------------
C
C ****  Clear error flag
C
      ERRSRC = EZS_SUCCESS
C
C ****  Check if an SRCP bank has been selected.
C
      IF ( ISRCP .LE. 0 ) THEN
        ERRSRC = EZS_BANK_NOTSELECTED
        GOTO 999
      ENDIF
C
C ****  Get bank address
C
      LSRCP = KSRCP(ISRCP)
C
C ****  Copy first word of each record into array NAMES
C       NOTE: Array NAMES MUST be ordered the same way as pointers
C       in order list
C
      LIDS = IC(LSRCP+JJIDS)    ! Number of identifiers
      WRDIDS = IC(LSRCP+JJNWRD) ! Number of words/identifier
C
      LPTI = LSRCP+IC(LSRCP+JJPIDS)-1 ! Base address of ID-list
      LPTO = LSRCP+IC(LSRCP+JJPORD)-1 ! Base address of order-list
C
      DO 350 I = 1,LIDS
        J = EZZAND(IC(LPTO+I),MASK)
        IPTI = LPTI + WRDIDS*(J-1)
        CALL UHTOC (IC(IPTI+1),4,IDENTF,NUMCHR)
        NAMES(I) = IDENTF
        ITYPE(I) = J ! Use as temporary buffer for order list
  350 CONTINUE
C
C ****  Re-order identifier pointers
C
      CALL SRTCHR (NAMES,LIDS,ITYPE)
C
C ****  Update order list
C
      MASK1 = EZZSHFT(MASK,NBITS)
      DO 360 I =  1,LIDS
        IC(LPTO+I) = EZZAND(IC(LPTO+I),MASK1)     ! Clear lower word
        IC(LPTO+I) = IC(LPTO+I) + ITYPE(I)      ! Put in new order list
  360 CONTINUE
C
 999  RETURN
      END
