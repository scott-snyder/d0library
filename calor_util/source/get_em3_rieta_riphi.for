      SUBROUTINE GET_EM3_RIETA_RIPHI(Ieta,Iphi,ILYR, RIETA, RIPHI) 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the half integer index of EM3 Layers. 
C-
C-   Inputs  : Ieta,Iphi,ILYR for Layers 3,4,5,6
C-   Outputs : RIETA, RIPHI = IETA, IPHI +/- off
C-   Controls: 
C-
C-   Created   8-AUG-1995   R. J. Genik II
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      Include 'd0$params:CAL_OFFLINE.PARAMS'
      Real poff,eoff,RIETA, RIPHI, off
      Parameter( off = 0.3)
      Integer Ieta,Iphi,ILYR
C----------------------------------------------------------------------
      RIeta = Real(IETA)
      RIPhi = Real(IPHI)
      If ((ILYR.LT.LYEM3A).or.(ILYR.GT.LYEM3D)) Return
      Goto (10,11,12,13), ILYR - 2
   10 poff = - off
      eoff = - off
      goto 20
   11 poff = + off
      eoff = - off
      goto 20
   12 poff = - off
      eoff = + off
      goto 20
   13 poff = + off
      eoff = + off
   20 RIeta = RIeta + Eoff
      RIphi = RIPhi + Poff
  999 RETURN
      END
