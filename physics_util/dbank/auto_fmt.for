      CHARACTER*(*) FUNCTION AUTO_FMT(DUM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns a format for printing a number
C-                         for use with MZFORM in zebra
C-                         Creates a Bank called FRMT which
C-                         is populated by the format type of the
C-                         word in question.
C-                   ITYPE  = 1 . Means Integer (I)
C-                          = 2.  Means Floating Point (F)
C-                          = 3.  Means Hollerith. (A)
C-
C-   Inputs  :
C-         LBANK = Bank address
C-        BNKLEN = Length of Bank
C-        IDATA  = Pointer into the bank of the current data whose format
C-                 is to be determined.
C-
C-   Outputs : Function value is the format
C-
C-   Created   8-JUN-1988   Michael Peters
C-   Updated  12-APR-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C
      REAL DUM,T
      INTEGER IT,IOK
      EQUIVALENCE (T,IT)
C
      INCLUDE 'D0$INC:AUTOF.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C-------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBQ.INC'         ! ZEBRA COMMON BLOCKS
      INCLUDE 'D0$INC:MZCA.INC'
      INCLUDE 'D0$INC:MZCB.INC'
C-------------------------------------------------------------------
C
      INTEGER ITYPE
C
      IF(LBANK.LE.0)RETURN
C TRY GET THE FORMAT FROM THE ZEBRA CHFORM
      IF(IOK.EQ.0)THEN
        T=QQ(KQS+LBANK+IDATA)    !This is the word to be formatted.
        ITYPE = IQQ(KQS+LFRMT+IDATA)    !TYPE FOR THIS WORD.
        IF(ITYPE.EQ.1.OR.ITYPE.EQ.2)THEN  ! Bit string or integer
          IF(ABS(IT).LT.1000000) THEN
            AUTO_FMT='(I5,I10)'
          ELSE
            AUTO_FMT='(I5,Z9,'' X'')'
          ENDIF
        ELSEIF(ITYPE.EQ.3)THEN            ! Floating point
          AUTO_FMT='(I5,G11.3)'
        ELSEIF(ITYPE.EQ.4)THEN            ! Double precision word 1
          AUTO_FMT='D1'
        ELSEIF(ITYPE.EQ.-4)THEN            ! Double precision word 2
          AUTO_FMT='D2'
        ELSEIF ( ITYPE.EQ.5 ) THEN
          AUTO_FMT='(I5,7X,A4)'
        ENDIF
      ELSE
        AUTO_FMT='(I5,Z9,'' U'')'         ! Undefined
      ENDIF
      RETURN
      END
