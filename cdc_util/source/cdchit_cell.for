C----------------------------------------------------------------------
      Subroutine CDCHIT_Cell(PhiMin,PhiMax,NCell,IAddr)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get addresses of cells potentially having
C-                         hits on the road.
C-
C-   Inputs  : PhiMin, PhiMax -- minimum and maximum Phi of the road
C-   Outputs : NCell - number of cells
C-             IAddr - array with encoded address, as in DHIT bank.
C-   Controls: None
C-
C-   Created   4-NOV-1993   Gregory L. Landsberg
C-   Updated  27-Jan-1996   sss - compile with g77.
C-
C----------------------------------------------------------------------
      Implicit        None
      Real            Pi, TwoPi, HalfPi, Rad
      Include        'D0$INC:PI.INC'
      Real            PhiMin, PhiMax
      Integer         NCell, IAddr(1), I1, I2, I, J, J1, ICell
C
      integer mask_fffc07ff
      data mask_fffc07ff / z'FFFC07FF' /
C
      NCell = 0
      If (PhiMin .gt. PhiMax) Return
C
      Do I = 0,3
        I1 = ICell(PhiMin,I)
        I2 = ICell(PhiMax,I)
        If (I1 .gt. I2) I1 = I1 - 32
C
        Do J = I1,I2
          NCell = NCell + 1
          If (J .ge. 0) Then
            J1 = J
          Else
            J1 = J + 32
          End If
          IAddr(NCell) = IOR(IOR(ISHFT(I,16),ISHFT(J1,11)),
     &                         mask_fffc07ff)
        End Do
      End Do
C
      Return
      End
