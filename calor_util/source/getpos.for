      SUBROUTINE GETPOS(ETA,PHI,ILR3,PADEDG,DEPTH,IOK,CELL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns Z,R positions for pad edges of the cells
C-
C-   Inputs  : ETA(4)   = IETA of cells
C-             PHI(4)   = IPHI of cells
C-             ILR3     = Layer no. for EM3 cell
C-   Outputs : PADEDG(4)= Z-coordinates of the left pad edges
C-             DEPTH(4) = R-coordinates of centers of corresponding cells
C-   Controls: IOK = 0  = Normal return
C-             IOK > 0  = Error code (same as in CELXYZ)
C-
C-   Created  19-APR-1995   Gregory L. Landsberg
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      Integer   Eta(4), Phi(4), ILR3, I, LYR, IOK
      Real      PadEdg(4), Depth(4), X, Y, Z, CELL(4)
      Real      RC, DR, ZC, DZ, AZ, DA
C
      Do I = 4,1,-1
        If (I .lt. 3) Then
          LYR = I
        Else If (I .eq. 3) Then
          LYR = ILR3
        Else
          LYR = 7
        End If
        If (ETA(I) .eq. 0) Then
          IF (I .ne. 4) Then
            ETA(I) = ETA(I+1)
            PHI(I) = PHI(I+1)
          Else                      ! There is always energy in layer 3
            ETA(I) = ETA(3)
            PHI(I) = PHI(3)
          End If
        End If
        CALL CELXYZ_FAST(ETA(I),PHI(I),LYR,X,Y,Z,IOK)
        If (IOK .ne. 0) Then
C          Type *,ETA(I),PHI(I),LYR
          Return
        End If
        If (ABS(ETA(I)) .le. 12) Then
          DEPTH(I) = SQRT(X**2+Y**2)
        Else
          DEPTH(I) = Z
        End If
        If (I .eq. 3) CALL CELXYZ_FAST(ETA(I),PHI(I),ILR3,X,Y,Z,IOK)
        If (IOK .ne. 0) Return
C
        Call CELL_WIDTH(ETA(I),PHI(I),LYR,RC,DR,ZC,DZ,AZ,DA,IOK)
        If (IOK .ne. 0) Return
        CELL(I)=DZ
        PADEDG(I) = Z - CELL(I)/2.
      End Do
C----------------------------------------------------------------------
  999 RETURN
      END
