      SUBROUTINE ISZEB
C-----------------------------------------------------------------------
C-
C-    Initialize the D0 zebra banks, /ZEBCOM/ in Geant.   The /ZEBCOM/
C-  bank structure shoud be created first, then geant bank structure
C-  should be generated.    This routine is called by s/r GZEBRA.
C-
C-    INPUT(A):   (none)
C-    INPUT(C):   IRDUNI,IRDREC,IRDCHR
C-                IWRUNI,IWRREC,IWRCHR from /ZEBIO/
C-                currentry variables above are set in this program.
C-    INPUT(B):   (none)
C-
C-    OUTPUT:     (none)
C-
C-    S.Kunori     Apr.,1986
C-    A.Jonckheere 6/24/86      Mod for Geant v3.10.61
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZEBIO.INC'
C
C  Initializing store in /ZEBCOM/ common...
C
      IXCOM = 0
C
      CALL MZSTOR(IXCOM,'/ZEBCOM/','Q',FENCE,LHEAD,LREF,ZSTOR,
     1   ZSTOR(10000),ENDZS)
      IXMAIN = IXCOM + 2
C
      END
