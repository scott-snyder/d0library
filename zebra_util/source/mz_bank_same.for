      FUNCTION MZ_BANK_SAME(L1,L2,IN_ZEBCOM,STATUS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : compare two zebra banks (both in ZEBCOM or ZEBSTP)
C-
C-   Returned value  : TRUE means banks are identical
C-   Inputs  : L1,L2 are the links for the two banks
C-             IN_ZEBCOM = .TRUE. if the two banks are in /ZEBCOM/
C-                 = .FALSE. if the two banks are in /ZEBSTP/  
C-   Outputs : STATUS = 0 banks are the same   
C                       1 neither bank exists 
C                      -1 can't find bank 1
C                      -2 can't find bank 2
C                      -3 banks have different data length
C                      -4 same length, different data contents
C                      -5 same data contents, different numbers of links
C                      -6 same data contents and # links; bank name differs
C              MZ_BANK_SAME = .TRUE. if banks are the same (status 0 or 1)
C-   Controls: 
C-
C-   Created  22-JUL-1992   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER L1,L2,STATUS,LEN,I
      LOGICAL IN_ZEBCOM,MZ_BANK_SAME
C----------------------------------------------------------------------
      MZ_BANK_SAME = .FALSE.
      STATUS = -4           !different until proven otherwise
C...crude validity checks
      IF ((L1.EQ.0).AND.(L2.EQ.0)) THEN
        STATUS = 1    ! This will return that banks are same
        GO TO 999
      ENDIF
      IF (L1.LE.0) THEN
        STATUS = -1
        GO TO 999
      ENDIF
      IF (L2.LE.0) THEN
        STATUS = -2
        GO TO 999
      ENDIF
C...look at contents
      IF ( IN_ZEBCOM ) THEN     
C...compare length
        LEN = IQ(L1-1)
        IF (LEN.LE.1) THEN
          STATUS = -1
          GO TO 999
        ENDIF
        IF (IQ(L2-1).NE.LEN) THEN
          STATUS = -3
          GO TO 999
        ENDIF
C...compare contents
        DO I = 1,LEN
          IF (IQ(L1+I).NE.IQ(L2+I)) GO TO 999
        ENDDO
        STATUS = 0        !good so far
C...compare links
        IF (IQ(L1-2).NE.IQ(L2-2)) STATUS = -5
        IF (IQ(L1-3).NE.IQ(L2-3)) STATUS = -5
C...compare bank name
        IF (IQ(L1-4).NE.IQ(L2-4)) STATUS = -6
      ELSE 
C
C...in ZEBSTP
C...compare length
        LEN = IC(L1-1)
        IF (LEN.LE.1) THEN
          STATUS = -1
          GO TO 999
        ENDIF
        IF (IC(L2-1).NE.LEN) THEN
          STATUS = -3
          GO TO 999
        ENDIF
C...compare contents
        DO I = 1,LEN
          IF (IC(L1+I).NE.IC(L2+I)) GO TO 999
        ENDDO
        STATUS = 0        !good so far
C...compare links
        IF (IC(L1-2).NE.IC(L2-2)) STATUS = -5
        IF (IC(L1-3).NE.IC(L2-3)) STATUS = -5
        IF (STATUS.NE.0) GO TO 999
C...compare bank name
        IF (IC(L1-4).NE.IC(L2-4)) STATUS = -6
      ENDIF
  999 CONTINUE
      MZ_BANK_SAME = STATUS.GE.0
      RETURN
      END
