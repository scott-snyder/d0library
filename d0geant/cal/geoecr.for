      SUBROUTINE GEOECR
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Sets up Endcap Cryostat Geometry for GEANT
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  22-Nov-1988   Elliott A. Treadwell
C-   Updated   1-APR-1989   Chip Stewart  new bellows shape is cylinder
C-    
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IVOLU,IT,IZ
C
      INCLUDE 'D0$INC:CRYVLN.INC/LIST'
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INCLUDE 'D0$INC:SRCPR.INC/LIST'
C
C----------------------------------------------------------------------
C Names of Cryostat Volumes in SRCP
C
      CHARACTER*32 ENDNM(NECRVL,2)  !EC SRCP names
C
C
      DATA ENDNM/
C
C ****  POSITIVE Z
C
     +          'CRY_END_WARM_BULGE+Z',                  
     +          'CRY_END_WARM_BACK+Z',                   
     +          'CRY_END_WARM_FRONT+Z',                  
     +          'CRY_END_WARM_RING_INNER+Z',             
     +          'CRY_END_WARM_RING_OUTER+Z',             
     +          'CRY_END_WARM_RING_LOWER+Z',             
     +          'CRY_END_WARM_BELLOW_H+Z',               
     +          'CRY_END_WARM_BELLOW_V1+Z',              
     +          'CRY_END_WARM_BELLOW_V2+Z',              
     +          'CRY_END_WARM_BEAM_TUBE+Z',              
     +          'CRY_END_WARM_FLANGE_H+Z',               
     +          'CRY_END_WARM_FLANGE_V+Z',               
C
     +          'CRY_END_COLD_BULGE+Z',             
     +          'CRY_END_COLD_FRONT+Z',             
     +          'CRY_END_COLD_BACK+Z',              
     +          'CRY_END_COLD_RING_INNER+Z',        
     +          'CRY_END_COLD_RING_OUTER+Z',        
     +          'CRY_END_COLD_RING_LOWER+Z',        
     +          'CRY_END_COLD_BELLOW_H+Z',          
     +          'CRY_END_COLD_BELLOW_V1+Z',         
     +          'CRY_END_COLD_BELLOW_V2+Z',         
     +          'CRY_END_COLD_BEAM_TUBE+Z',         
C
C ****  NEGATIVE Z
C
     +           'CRY_END_WARM_BULGE-Z',             
     +           'CRY_END_WARM_BACK-Z',              
     +           'CRY_END_WARM_FRONT-Z',             
     +           'CRY_END_WARM_RING_INNER-Z',        
     +           'CRY_END_WARM_RING_OUTER-Z',        
     +           'CRY_END_WARM_RING_LOWER-Z',        
     +           'CRY_END_WARM_BELLOW_H-Z',          
     +           'CRY_END_WARM_BELLOW_V1-Z',         
     +           'CRY_END_WARM_BELLOW_V2-Z',         
     +           'CRY_END_WARM_BEAM_TUBE-Z',         
     +           'CRY_END_WARM_FLANGE_H-Z',          
     +           'CRY_END_WARM_FLANGE_V-Z',          
C
     +           'CRY_END_COLD_BULGE-Z',             
     +           'CRY_END_COLD_FRONT-Z',             
     +           'CRY_END_COLD_BACK-Z',              
     +           'CRY_END_COLD_RING_INNER-Z',        
     +           'CRY_END_COLD_RING_OUTER-Z',        
     +           'CRY_END_COLD_RING_LOWER-Z',        
     +           'CRY_END_COLD_BELLOW_H-Z',          
     +           'CRY_END_COLD_BELLOW_V1-Z',         
     +           'CRY_END_COLD_BELLOW_V2-Z',         
     +           'CRY_END_COLD_BEAM_TUBE-Z' /       
C
C-----------------------------------------------------------------------
C  End Cap Cryostat
C
C  EC CRYOSTAT VOLUMES
C
        DO 40 IZ = 1,2  !Do both  +/- Z
          DO 50 IT = 1,NECRVL
            CALL VOLPOS(ENDNM(IT,IZ))
            ECRYVL(IT,IZ)= ISRCPR(1)
   50     CONTINUE
   40   CONTINUE
C
C
      END
