C DEC/CMS REPLACEMENT HISTORY, Element J2STRG.FOR
C *1     3-JUN-1992 13:00:36 LUPE "Release of DI3GL after split"
C DEC/CMS REPLACEMENT HISTORY, Element J2STRG.FOR
      SUBROUTINE J2STRG(STRING)
      CHARACTER*(*) STRING
C  SET LINE WIDTH LARGER
      CALL J3STRG(STRING)
C  RESTORE LINE WIDTH
      END