      FUNCTION BKLEGO(TITLE,NTRIPL,LLEGO)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-        book a LEGO plot bank
C-
C-   Input  :
C-       TITLE= title (at most 16 characters are used)
C-       NTRIPL = number of triplets
C-   Output:
C-       LLEGO  = pointer to newly created bank
C-
C-   ENTRY GZLEGO(NUMB): integer function, return pointer to
C-                       LEGO bank number NUMB
C-
C-   Created  14-FEB-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      CHARACTER*(*) TITLE
      CHARACTER*32 TITL1
      INTEGER NTRIPL,LLEGO,BKLEGO,GZLEGO,NUMB
      LOGICAL FIRST
      INTEGER LEGO,NLEGO,IOH,ND,JBIAS,N,LZLAST,LZLOC
      SAVE NLEGO,FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF(FIRST) THEN
        CALL MZFORM('LEGO','1I 4H -F',IOH)
        CALL GSLINK('BKLEGO',NLEGO) ! reserve a link
        LEGO=LZLOC(IXMAIN,'LEGO',1) ! look for pre-existing bank
        FIRST=.FALSE.
      ENDIF
C
      LEGO=LSLINK(NLEGO)
      ND=3*NTRIPL+10
C
      IF(LEGO.EQ.0) THEN
        JBIAS=1
      ELSE
        JBIAS=0
        IF(LQ(LEGO).NE.0) LEGO=LZLAST(0,LEGO)
      ENDIF
C
      CALL MZBOOK(IXMAIN,LEGO,LEGO,JBIAS,'LEGO',0,0,
     &  ND,IOH,0)
      IF(JBIAS.EQ.1) THEN
        LSLINK(NLEGO)=LEGO
        IQ(LEGO-5)=1
      ENDIF
      IQ(LEGO+1)=1     ! version number
      IQ(LEGO+2)=NTRIPL
      TITL1=TITLE
      CALL UCTOH(TITL1,IQ(LEGO+3),32,32)
      LLEGO=LEGO
      BKLEGO=0            ! this is a dummy value to satisfy F77 standards
  999 RETURN
C
C
      ENTRY GZLEGO(NUMB)
C
      IF(FIRST) THEN
        CALL INZLNK     ! initialize general link area
        CALL GSLINK('BKLEGO',NLEGO) ! reserve a link
        LEGO=LZLOC(IXMAIN,'LEGO',1) ! look for pre-existing bank
        FIRST=.FALSE.
      ENDIF
C
      LEGO=LSLINK(NLEGO)
C
      IF ( LEGO.NE.0 ) THEN
    1   N=IQ(LEGO-5)
        IF(N.NE.NUMB) THEN  ! loop over linear structure
          LEGO=LQ(LEGO)
          IF(LEGO.NE.0) GOTO 1
        ENDIF
      ENDIF
C
      GZLEGO=LEGO
      RETURN
      END
