      SUBROUTINE check_em_quality(lclus,usermask,ok)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : uses USERMASK to check the quality of
C-                         electron/photon cluster
C-
C-   Inputs  : LCLUS : Link to electron, photon cluster
C-             USERMASK = user define electron/photon quality mask
C-
C-   Outputs : OK = .true. if electron satifies USERMASK
C-   Controls: None
C-   ****NOTE**** The latest status bit info can be found in
C-                D0$CALOR_OFF$SOURCE:CLEANEM.FOR
C-              STATUS BIT   0 : coarse Hmatrix chisquared
C-              STATUS BIT   1 : fine H matrix chisquared
C-              STATUS BIT   2 : CC em flag
C-              STATUS BIT   3 : cluster EM fraction
C-              STATUS BIT   4 : core energy cut
C-              STATUS BIT   5 : transverse dispersion cut
C-              STATUS BIT   6 : sigma5-sigma3
C-              STATUS BIT   7 : Isolation cut Energy (Cone 1)
C-              STATUS BIT   8 : Isolation cut Energy (Cone 2)
C-              STATUS BIT   9 : Isolation cut ET (Cone 1)
C-              STATUS BIT  10 : Isolation cut ET (Cone 2)
C-              STATUS BIT  11 : Close to crack flag
C-              STATUS BIT  12 : Number of cells below minimum
C-              STATUS BIT  13 : L2/RECO match
C-              STATUS BIT  14 : Spare
C-              STATUS BIT  15 : Spare
C-              STATUS BIT  16 : Distance 1 cut (Rdeltaphi for CC and EC)
C-              STATUS BIT  17 : Distance 2 cut (Delta Z for CC delta R for EC
C-              STATUS BIT  18 : Shower centroid/track match significance
C-              STATUS BIT  19 : Set if another track in the road passes
C-                               track match significance criterion
C-              STATUS BIT  20 : number of tracks in a cone of dR
C-              STATUS BIT  21 : spare
C-              STATUS BIT  22 : CDC ionization (MIP)
C-              STATUS BIT  23 : FDC ionization (MIP)
C-              STATUS BIT  24 : VTX chamber ionization (MIP)
C-              STATUS BIT  25 : TRD information available (=1 if problem)
C-              STATUS BIT  26 : TRD truncated mean cut
C-              STATUS BIT  27 : spare
C-              STATUS BIT  28 : Vertex Transverse impact parameter (in x,y)
C-              STATUS BIT  29 : Vertex Z impact parameter (in Z)
C-
C-   Created  27-OCT-1992   Meenakshi Narain
C-   Updated  10-DEC-1992   Meenakshi Narain  make status word compatible 
C-                                            with different versions of
C-                                            pelc/ppho 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LCLUS,STATUS,IER,USERMASK,VERSION
      LOGICAL OK,FIRST
      DATA FIRST/.TRUE./

C----------------------------------------------------------------------
      OK = .TRUE.
      VERSION = IQ(LCLUS+1)
      IF (VERSION.EQ.1) THEN
        STATUS = IQ(LCLUS+20)
      ELSE IF (VERSION.EQ.2) THEN
        IF(IQ(LCLUS-4).EQ.4HPPHO) THEN
          STATUS = IQ(LCLUS+23)
        ELSE
          STATUS = IQ(LCLUS+20)
        ENDIF
      ELSE
        STATUS = IQ(LCLUS+30)
      ENDIF
      IF (IAND(STATUS,USERMASK).NE.0) THEN
        OK = .FALSE.
      ENDIF

  999 RETURN
      END
