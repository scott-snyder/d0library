C DEC/CMS REPLACEMENT HISTORY, Element CC_MOTHER.INC
C *1     8-DEC-1988 14:45:08 FUESS "Initial entry of CC .INC files"
C DEC/CMS REPLACEMENT HISTORY, Element CC_MOTHER.INC
C----------------------------------------------------------------------
C-
C-      /CC_MOTHER/
C-
C-      CC Mother Volume parameters
C-
C-      The mother volume will be two 'TUBEs' with inner radius given
C-      by the outer radius of the cryostat inner cold wall, outer
C-      radius given by the cryostat outer cold wall, length given by
C-      the maximum module length, and filled with liquid argon.
C-
C-
C-   Created  28-NOV-1988   Stuart Fuess
C-
C----------------------------------------------------------------------
      REAL CC_MOTHER_INNER_RADIUS       
      REAL CC_MOTHER_OUTER_RADIUS       
      REAL CC_MOTHER_LENGTH             
      INTEGER CC_MOTHER_MATERIAL        
      COMMON /CC_MOTHER/ CC_MOTHER_INNER_RADIUS,
     &                   CC_MOTHER_OUTER_RADIUS,
     &                   CC_MOTHER_LENGTH,
     &                   CC_MOTHER_MATERIAL
