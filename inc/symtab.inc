*
* Included file: SYMTAB.INC - contains definition for symbol table for the
*                RZ query language.
*
* SYMCHR contains the character string representations of the identifiers
* SYMDSC contains the type descriptor information - current values of the
*        descriptor are 1- integer or 2 real 3 bit-string 4 Hollerith (char)
*
      INTEGER  REALTY, INTTYP, MAXSYM, BITTYP, HOLTYP

      PARAMETER (MAXSYM = 11)
      CHARACTER*8 SYMCHR(1:MAXSYM)
      INTEGER  SYMDSC(1:MAXSYM)                                     
*
* The types are: INTEGER, REAL, BIT-string and HOLlerith (character)
*
      PARAMETER (INTTYP = 1)
      PARAMETER (REALTY = 2)
      PARAMETER (BITTYP = 3)
      PARAMETER (HOLTYP = 4)
*      
