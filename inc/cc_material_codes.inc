C DEC/CMS REPLACEMENT HISTORY, Element CC_MATERIAL_CODES.INC
C *1     8-DEC-1988 14:44:02 FUESS "Initial entry of CC .INC files"
C DEC/CMS REPLACEMENT HISTORY, Element CC_MATERIAL_CODES.INC
C----------------------------------------------------------------------
C-
C-      CC Materials
C-
C-   Created  10-NOV-1988   Stuart Fuess
C-
C----------------------------------------------------------------------
      INTEGER CODE_HOLLERITH(5)
      CHARACTER*32 CODE_CHARACTER(5)
      DATA CODE_CHARACTER / 'URANIUM',
     &                      'STAINLESS_STEEL',
     &                      'COPPER',
     &                      'G10',
     &                      'LIQUID_ARGON' /
      DATA CODE_HOLLERITH / 1HU,
     &                      2HSS,
     &                      2HCU,
     &                      3HG10,
     &                      4HLIQA /
