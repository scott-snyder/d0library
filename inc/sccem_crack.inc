C----------------------------------------------------------------------
C-
C-      /SCCEM_CRACK/
C-
C-      CCEM Crack information, giving the volume of each of 4 types
C-      of material:
C-              1 = Uranium
C-              2 = Stainless Steel
C-              3 = Copper
C-              4 = G10
C-      
C-   Created  26-OCT-1988   Stuart Fuess
C-
C----------------------------------------------------------------------
      REAL CCEM_CRACK_VOL(4)            
      REAL CCEM_NON_CRACK_VOL           
      INTEGER CCEM_CRACK_MATERIAL       
      COMMON /SCCEM_CRACK/ CCEM_CRACK_VOL,
     &                    CCEM_NON_CRACK_VOL,
     &                    CCEM_CRACK_MATERIAL
