C----------------------------------------------------------------------
      Integer Function ICell(Phi,Layer)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return Cell number of layer Layer (0..3),
C-                         corresponding to given Phi.
C-
C-   Inputs  : Phi - Unnormalized Phi angle
C-             Layer - Layer number (0..3)
C-   Outputs : ICell - Cell number (0..31)
C-   Controls: None
C-
C-   Created   8-DEC-1993   Gregory L. Landsberg
C-   Updated  21-AUG-1994   Gregory L. Landsberg - fixed a bug in cell no.
C-
C----------------------------------------------------------------------
      IMPLICIT    NONE
      Integer     Layer
      Real        Phi, Phi0, Pi, HalfPi, TwoPi, Rad
      Include    'D0$INC:PI.INC'
      Real        HalfCell, Cell
      Parameter ( HalfCell = Pi/32., Cell = Pi/16. )
C
      If ((Layer/2)*2 .ne. Layer) Then
        Phi0 = Phi
      Else
        Phi0 = Phi + HalfCell
      End If
C
      If (Phi0 .lt. 0) Then
        Phi0 = Phi0 + TwoPi
      Else If (Phi0 .ge. TwoPi) Then
        Phi0 = Phi0 - TwoPi
      End If
C
      ICell = Phi0/Cell
C----------------------------------------------------------------------
  999 RETURN
      END
