      SUBROUTINE MUZBND(IQQUAD,ZBEND)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
CC    RETURNS BEND POINT FOR A GIVEN QUADRANT
CC
CC    D HEDIN 11/88 
CC    DH 5/89 CHANGE CENTRAL
CC    DH 3/90 change ends
CC    DH 4/91 change QUAD=0
CC    DH 10/91 CHANGE QUAD USAGE
CC    Atsushi Taketani 9/92 Use STP information
CC                          Currently use Slab 1,2,3,4 for central,
CC                          Slab 21 for south, Slab 25 for north.
CC    This routine dose not support any tilt on magnet.
CC    AT,TD 12/92 PROTECTION AND WARNING againt missing banks
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      INTEGER IQUAD,IQQUAD
      REAL ZBEND
      CHARACTER*4 HSHAPE
      CHARACTER*60 ERROR_MESSAGE
      INTEGER NSPAR,NBUF,IBUF
      REAL    SPAR(3),XPAR(3),ROTM(3,3)
      INTEGER LMMAH,GZMMAH
      LOGICAL FIRST
      DATA    FIRST /.TRUE./
C
      IQUAD=MOD(IQQUAD,100)
C
      LMMAH = GZMMAH(1)
      IF ( LMMAH.EQ.0 ) THEN
        IF ( FIRST ) THEN
          ERROR_MESSAGE = 'MISSING MMAH BANK, USE DEFAULT VALUE'
          CALL ERRMSG('MUON_L2','MUZBND',ERROR_MESSAGE,'W')
          FIRST = .FALSE.
        END IF
        IF(IQUAD.EQ.1.OR.IQUAD.EQ.2) THEN      ! CENTRAL +
          ZBEND=364.49
        ELSE IF(IQUAD.EQ.3.OR.IQUAD.EQ.4) THEN  ! CENTRAL -
          ZBEND=-364.49
        ELSE IF(IQUAD.GE.5.AND.IQUAD.LE.8) THEN ! NORTH END
          ZBEND=-525.62
        ELSE IF(IQUAD.GE.9.AND.IQUAD.LE.12) THEN ! SOUTH END
          ZBEND=525.62
        ELSE IF(IQUAD.EQ.0) THEN         ! BASEMENT
          ZBEND=170.5
        ENDIF
      ELSE      
        IF ( IQUAD.GE.1.AND.IQUAD.LE.4 ) THEN  ! Central
          CALL MUMAGS(IQUAD,HSHAPE,NSPAR,SPAR,XPAR,ROTM,NBUF,IBUF)
          IF ( MOD(IQUAD,2).EQ.0 ) THEN        ! central side
            ZBEND = XPAR(2)
          ELSE
            ZBEND = XPAR(1)                    ! central top/bottom
          END IF
        ELSE IF ( IQUAD.GE.5.AND.IQUAD.LE.8 ) THEN ! North end
          CALL MUMAGS(25,HSHAPE,NSPAR,SPAR,XPAR,ROTM,NBUF,IBUF)
          ZBEND = XPAR(3)
        ELSE IF ( IQUAD.GE.9.AND.IQUAD.LE.12 ) THEN ! South end
          CALL MUMAGS(21,HSHAPE,NSPAR,SPAR,XPAR,ROTM,NBUF,IBUF)
          ZBEND = XPAR(3)
        ELSE IF ( IQUAD.EQ.0 ) THEN            ! basement
          ZBEND = 170.5
        ELSE                                   ! invalid quadrant
          ZBEND = -99999.0
        END IF
      END IF   	
C
 999  RETURN
      END
