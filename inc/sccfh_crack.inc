C----------------------------------------------------------------------
C-
C-      /SCCFH_CRACK/
C-
C-      CCFH Crack information, giving the volume of each of 4 types
C-      of material:
C-              1 = Uranium
C-              2 = Stainless Steel
C-              3 = Copper
C-              4 = G10
C-      
C-   Created  26-OCT-1988   Stuart Fuess
C-
C----------------------------------------------------------------------
      REAL CCFH_CRACK_VOL(4)            
      REAL CCFH_NON_CRACK_VOL           
      INTEGER CCFH_CRACK_MATERIAL       
      COMMON /SCCFH_CRACK/ CCFH_CRACK_VOL,
     &                    CCFH_NON_CRACK_VOL,
     &                    CCFH_CRACK_MATERIAL
