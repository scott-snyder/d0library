C----------------------------------------------------------------------
C-
C-      /SCCCH_CRACK/
C-
C-      CCCH Crack information, giving the volume of each of 4 types
C-      of material:
C-              1 = Uranium
C-              2 = Stainless Steel
C-              3 = Copper
C-              4 = G10
C-      
C-   Created  26-OCT-1988   Stuart Fuess
C-
C----------------------------------------------------------------------
      REAL CCCH_CRACK_VOL(4)            
      REAL CCCH_NON_CRACK_VOL           
      INTEGER CCCH_CRACK_MATERIAL       
      COMMON /SCCCH_CRACK/ CCCH_CRACK_VOL,
     &                    CCCH_NON_CRACK_VOL,
     &                    CCCH_CRACK_MATERIAL
