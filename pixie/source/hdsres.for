      SUBROUTINE HDSRES
C ASCII escape character
      INTEGER ESCAPE
      PARAMETER ( ESCAPE = 27 )
C
C 1 - Reset terminal (DECSTR)
C
      CHARACTER*4 DECSTR
      CHARACTER*6 HDSSTR
C
      DECSTR(1:1) = CHAR(ESCAPE)
      HDSSTR(1:1) = CHAR(ESCAPE)
      HDSSTR(2:6) = '[?38l'
      DECSTR(2:4) = '[!p'
C Output of the above string should reset the terminal
C (See VT240 Programmer Pocket Guide, p.47)
C *** Note CSI = ESCAPE  + [
      TYPE*, DECSTR
      TYPE*, HDSSTR
      RETURN
      END
