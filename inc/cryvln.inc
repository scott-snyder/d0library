C----------------------------------------------------------------------
C-   File name:   CRYVLN.INC
C-   Purpose:
C-      This common block keeps a list of all Geant volume names in the
C-      cryostat that are NOT Mother volumes.
C-
C-   Created  14-NOV-1988   Elliott A. Treadwell, Harrison B. Prosper
C-                          Central cryostat NOW symmetric about Z = 0
C-                          
C-   Updated   1-APR-1989   Chip Stewart  bellows cylinder 
C----------------------------------------------------------------------
C
C ****  CRYOSTAT VESSEL
C
      INTEGER NUCRVL,NECRVL
      PARAMETER( NUCRVL = 6 ) 
C                             
      PARAMETER( NECRVL = 22 )
C
      COMMON/CRYVLN/UCRYVL(NUCRVL,2),ECRYVL(NECRVL,2)
      INTEGER UCRYVL,ECRYVL
