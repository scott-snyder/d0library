      SUBROUTINE GTMPHT(IHIT,ICELLHIT,SIGN,X,Y,Z)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Obtains the X,Y,Z coordinates of ANY hit in MUOH.
C-                         Useful for hits NOT on track.
C-
C-   Inputs  :  
C-      IHIT-    [INTEGER]- Hit number in MUOH.  Example: IHIT=1,2,3,...
C-                not 1,26,51,...
C-      ICELLHIT-[INTEGER]-Hit number within a cell. CELLHIT = 1 or 2.
C-      SIGN-    [INTEGER]-The positive/negative ambiguity of DRIFT
C-                        SIGN=-1 NEGATIVE AMBIGUITY
C-                            =+1 POSITIVE     "
C-                            =0  NEGLECT AMBIGUITY--DRIFT COORD. @ WIRE
C-   Outputs :
C-      X,Y,Z-   [REAL]-Coordinates of the hit.
C-   Controls:
C-
C-   Created  07-NOV-1990   Cary Y. Yoshikawa
C-   Updated  31-May-1991   C.Y.
C-              -added IABS to ORENT
C-              -killed SIGN for TDIV
C-   DH 2/92 28 WORDS/MUOH HIT
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
C    Local Declarations:
C    ===================
C
      INTEGER IHIT,ICELLHIT,SIGN,LMUOH,GZMUOH,ORENT
      REAL X,Y,Z,DRIFT,TDIV
C
C----------------------------------------------------------------------
C    Executable Code:
C    ================
      LMUOH=GZMUOH(0)
      DRIFT=Q(LMUOH+14+ICELLHIT+28*(IHIT-1))
      TDIV=Q(LMUOH+16+ICELLHIT+28*(IHIT-1))
      ORENT=IABS(IQ(LMUOH+5+28*(IHIT-1)))
      IF (ORENT.EQ.1) THEN
        X=Q(LMUOH+21+28*(IHIT-1))
        Y=Q(LMUOH+22+28*(IHIT-1))+TDIV
        Z=Q(LMUOH+23+28*(IHIT-1))+SIGN*DRIFT
      ELSEIF (ORENT.EQ.2) THEN
        X=Q(LMUOH+21+28*(IHIT-1))+TDIV
        Y=Q(LMUOH+22+28*(IHIT-1))
        Z=Q(LMUOH+23+28*(IHIT-1))+SIGN*DRIFT
      ELSEIF (ORENT.EQ.3) THEN
        X=Q(LMUOH+21+28*(IHIT-1))+SIGN*DRIFT
        Y=Q(LMUOH+22+28*(IHIT-1))+TDIV
        Z=Q(LMUOH+23+28*(IHIT-1))
      ELSEIF (ORENT.EQ.4) THEN
        X=Q(LMUOH+21+28*(IHIT-1))+TDIV
        Y=Q(LMUOH+22+28*(IHIT-1))+SIGN*DRIFT
        Z=Q(LMUOH+23+28*(IHIT-1))
      ENDIF
      RETURN
      END
