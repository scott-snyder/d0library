      SUBROUTINE BKVERT(LVERT,NR)
C------------------------------------------------------------------
C 
C  Book a vertex bank VERT
C 
C  Daria Zieminska Nov. 1989
C  Modified 17-JUL-1991  Tom Trippe, add 4 wds for phis, thetas
C-   Updated  30-SEP-1991   Qizhong Li-Demarteau  fix wrong MZFORM from
C-                                     "12F" to "16F" and added a check
C-                                     for LVERH and added SAVE statement
C-   Updated  29-OCT-1991   Qizhong Li-Demarteau  added linear bank 
C-                                                booking in sequence 
C-   Updated  21-JUL-1995   Srini Rajagopalan  Increase number of words to 19.
C-                          Word 19 = Number of reco objects pointing to this
C-                          vertex. (Filled by VERTEX_FIX) 
C-   Updated   5-SEP-1995   Srini Rajagopalan  Move Version Number filling to
C-                          this routine. Make addition of Word 19 as version 2 
C-   Updated  25-SEP-1995   Srini Rajagopalan  Use LZLAST while booking bank 
C                            
C------------------------------------------------------------------
      IMPLICIT NONE  
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
      INCLUDE 'D0$LINKS:IZVERT.LINK/LIST'                             
      INTEGER LVERH,GZVERH,LVERT,NR
      INTEGER LZLAST
      INTEGER IXVERT,MBOOKT(5) 
      LOGICAL FIRST 
      SAVE FIRST
      DATA FIRST/.TRUE./
C------------------------------------------------------------------
      IF (FIRST) THEN
        CALL MZFORM('VERT','1I 1B 17F',IXVERT)
        CALL UCTOH('VERT',MBOOKT(1),4,4)
        MBOOKT(3)=1
        MBOOKT(4)=19
        MBOOKT(5)=IXVERT
        FIRST=.FALSE.
      END IF
      MBOOKT(2)=NR+MBOOKT(3)      
      LVERH=GZVERH()
      IF (LVERH .GT. 0) THEN
        LVERT = LQ(LVERH - IZVERT)
        IF (LVERT .LE. 0) THEN
          CALL MZLIFT(IXMAIN,LVERT,LVERH,-IZVERT,MBOOKT,0)
          IQ(LVERT - 5) = 1
        ELSE
          LVERT = LZLAST(IXMAIN,LVERT)
          CALL MZLIFT(IXMAIN,LVERT,LVERT,0,MBOOKT,0)
        ENDIF
      ELSE
        LVERT = 0
      ENDIF
C
      IQ(LVERT+1) = 2                     ! version number
C
  999 RETURN
      END       
