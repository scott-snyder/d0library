      SUBROUTINE WRHEAD(OUNIT)
C------------------------------------------------------------------
C-                                                                -
C-  Write header record                                           -
C-                                                                -
C-   INPUT:                                                       -
C-    OUNIT= ZEBRA output file unit number                        -
C-                                                                -
C-     SDP Nov.,1986                                              -
C-                                                                -
C------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER OUNIT,IRECTP
C
C until MZCOPY becomes available header record will have to
C be written from division 2
C
      IRECTP=MOD(IQ(LHEAD+1),1000)     ! record type
      IF(IRECTP.EQ.1)
     $  CALL FZOUT(OUNIT,1,LHEAD,1,' ',1,0,0)
      RETURN
      END    
