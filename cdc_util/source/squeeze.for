C----------------------------------------------------------------------
      Subroutine Squeeze(I,NH,IH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Wipes out the element I from array IH
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  17-NOV-1993   Gregory L. Landsberg
C-
C----------------------------------------------------------------------
      Implicit        None
      Integer         I,NH,IH(1),K
C
      If (I .gt. NH) Return
      NH = NH-1
      Do K = I,NH
        IH(K) = IH(K+1)
      End Do
      I = I - 1
C----------------------------------------------------------------------
      Return
      End
