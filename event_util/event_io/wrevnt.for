      SUBROUTINE WREVNT(OUNIT)
C-----------------------------------------------------------------
C-                                                               -
C-    INPUT:                                                     -
C-    OUNIT = output unit number                                 -
C-                                                               -
C-    SDP Jan.,1987                                              -
C-    sss 10/93 - supply proper mzform for user header.          -
C-                                                               -
C-----------------------------------------------------------------
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER OUNIT
      INTEGER NMAX
      PARAMETER (NMAX=50)
      INTEGER IUHEAD(NMAX),NUHEAD,ND
      CHARACTER*100 HEAD_FORM
      INTEGER HEAD_FORM_LEN

      INTEGER MZFORM_VECTOR_SIZE
      PARAMETER (MZFORM_VECTOR_SIZE = 17)
      INTEGER MZFORM_VECTOR(MZFORM_VECTOR_SIZE)
C
      IF(OUNIT*LHEAD.EQ.0) GO TO 999 ! skip if OUNIT=0 or LHEAD=0
C
C  write out LHEAD banks...
C
      ND=IQ(LHEAD-1)
      IF(ND.GT.NMAX) ND=NMAX
      DO 100 NUHEAD=1,ND
        IUHEAD(NUHEAD)=IQ(LHEAD+NUHEAD)
  100 CONTINUE
      CALL MZIOTC (IXMAIN, LHEAD, HEAD_FORM_LEN, HEAD_FORM)
      CALL MZIOCH (MZFORM_VECTOR, MZFORM_VECTOR_SIZE,
     &             HEAD_FORM(1:HEAD_FORM_LEN))
      CALL FZOUT(OUNIT,IXMAIN,LHEAD,1,' ', MZFORM_VECTOR, ND,IUHEAD)
  999 RETURN
      END
