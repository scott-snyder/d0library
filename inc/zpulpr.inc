C----------------------------------------------------------------------
C-  pulse shape simulation parameters for VTX, CDC, FDC
C-
C-   Created  14-JUN-1989   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      COMMON /ZPULPR/ PARTR(5,0:2), PARTW(5,0:2), TRLIMT(0:2), 
     &                TWLIMT(0:2), MINRS(0:2), MINWID(0:2), 
     &                PULWEI(0:2), DLSCAL(0:2), SCALE(0:2), NBPBIN
      REAL  PARTR, PARTW, TRLIMT, TWLIMT, MINRS, MINWID, SCALE
      REAL  PULWEI, DLSCAL, NBPBIN  
