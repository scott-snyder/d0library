      SUBROUTINE PJPTFL (ISAQID,POINTR,MAX_ISAQ,NMISAQ)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : For every PJET bank book and fill the pointer
C-   bank PJPT. Fill reference links in PJPT with ISAQ addresses.
C-
C-   Inputs  : ISAQID(*)        [I]     List of ISAQ bank ids
C-             POINTR(MAX_ISAQ) [I]     Reference pointer array
C-             MAX_ISAQ         [I]     Maximum array dimension
C-             NMISAQ           [I]     number of ISAQ parton in this PJET
C-   Outputs :
C-   Controls:
C-
C-   Created  19-JAN-1990   Boaz Klima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER MAX_ISAQ
      INTEGER ISAQID(*),POINTR(MAX_ISAQ),NMISAQ
      INTEGER ID,IP,GZISAQ,GZPJET,RFLINK,LZFIND,JET
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:LKPJET.INC'
C----------------------------------------------------------------------
C
C ****  LPJET address should be in LKPJET common
C
      IF ( LPJET .LE. 0 ) THEN
          CALL ERRMSG('ISAJET_UTIL','PJPTFL',
     &        'PJET not found','W')
          GOTO 999
      ENDIF
C
C ****  Get address of first ISAQ bank
C
      LISAQ=GZISAQ()
      IF ( LISAQ.LE.0 ) THEN
        CALL ERRMSG('ISAJET_UTIL','PJPTFL',
     &        'No ISAQ banks','W')
        GOTO 999
      ENDIF
C
C ****  BOOK AND FILL JET POINTER BANK FOR THIS JET
C
      CALL BKPJPT(LPJET,NMISAQ,LPJPT)
      DO IP = 1, NMISAQ
        ID = ISAQID ( POINTR(IP) ) ! Get ID of ISAQ bank
C
C ****  Given bank ID find address
C
        RFLINK = LZFIND(IXMAIN,LISAQ,ID,-5)     ! Get ISAQ address
        IF ( RFLINK .GT. 0 ) THEN
          LQ(LPJPT-IP-1) = RFLINK
        ELSE
          LQ(LPJPT-IP-1) = 0
          CALL ERRMSG('ISAJET_UTIL','PJPTFL',
     &        'Bad ISAQ bank numeric ID','W')
        ENDIF
      ENDDO
  999 RETURN
      END
