      SUBROUTINE UTAGFL(IDMAX,NDIMG,XTAGS,IXGRP,IGRP,NGRP,LUTAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill in the contents of the bank UTAG.
C-
C-   Inputs  :  IDMAX,NDIMG,XTAGS,IXGRP,IGRP,NGRP
C-
C-   Created  14-MAR-1993 00:20:40.59  Balamurali V.
C-   Updated  10-JUL-1994   Ulrich Heintz  bank version = 2 
C-   Updated  11-AUG-1994   Ian Adam  drop old UTAG when booking new UTAG 
C-   Updated  23-OCT-1995   Ulrich Heintz  bank version = 5 
C-   Updated  13-Jan-1996   sss - compile with g77.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER GZUTAG,LUTAG
      INTEGER I,J,K,L,POINTER,ND,IGRP,NGRP
      INTEGER IDMAX,NDIMG(IGRP)
      CHARACTER*8 XTAGS(IGRP,NGRP)
      INTEGER IXGRP(IGRP)
C----------------------------------------------------------------------
C Determine no of data words
C
      ND = 0
      DO J = 1,IDMAX
        DO K = 1,NDIMG(J)
          ND = ND + 2
        ENDDO
      ENDDO
      ND = ND + IDMAX + 2
      ND = ND + IDMAX             
C
C Book Bank
C
      LUTAG = GZUTAG()
      IF (LUTAG.GT.0) CALL MZDROP(IXCOM,LUTAG,'L')

      CALL BKUTAG(ND,LUTAG)
C
C Fill Bank
C
      IQ(LUTAG+1) = 5               ! bank version
      IQ(LUTAG+2) = IDMAX

      POINTER = LUTAG+2
      DO I = 1,IDMAX
        IQ(POINTER+I) = NDIMG(I)
      ENDDO

      L = 0
      POINTER = POINTER+IDMAX + 1
C
      DO J = 1,IDMAX 
        IQ(POINTER+L)=IXGRP(J)
        L = L + 1
      ENDDO
C
      DO J = 1,IDMAX
        DO K = 1,NDIMG(J)
          CALL SET_CHAR(POINTER+L,XTAGS(J,K))
          L = L + 2
        ENDDO
      ENDDO
C
  999 RETURN
      END
