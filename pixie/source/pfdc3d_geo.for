      SUBROUTINE PFDC3D_GEO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw FDC geometry in 3-dimensions.
C-   Amount of detail is determined by RCP parameter "FDC DRAW 3D DETAIL".
C-   Larger (+or-) results in greater Theta chamber detail (from +-1 to +-3).
C-   Phi chamber is draw if "FDC DRAW 3D DETAIL" is positive.
C-
C-   Inputs  : none
C-   Outputs : none
C-
C-   Created  23-JUN-1992   Robert E. Avery
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER HALF,QUAD
      INTEGER DRAW_FULL
      REAL    RIN, ROUT, ZIN, ZOUT 
      REAL    DIMEN(6)
C----------------------------------------------------------------------
      CALL EZPICK('PX_FDCDIS_RCP')
      CALL PUGETV( 'FDC DRAW 3D DETAIL', DRAW_FULL)        
      CALL EZRSET
C
      CALL PUOPEN
      DO HALF =  0, 1
        DO  QUAD =  0, 7
          CALL PFTQUAD_3D(HALF,QUAD)
        ENDDO
C
        IF ( DRAW_FULL.GE.1 ) THEN      ! Draw Phi cylinder
          CALL GTFWAL(3*HALF+2,DIMEN)
          RIN  = DIMEN(1)
          ROUT = DIMEN(2)
          ZIN  = DIMEN(6) - DIMEN(3)
          ZOUT = DIMEN(6) + DIMEN(3)
C
          CALL JCIRCL(0., 0., ZIN,  ROUT, 0)
          CALL JCIRCL(0., 0., ZOUT, ROUT, 0)
          CALL J3MOVE(0., ROUT, ZIN)
          CALL J3DRAW(0., ROUT, ZOUT)
          CALL J3MOVE(0.,-ROUT, ZIN)
          CALL J3DRAW(0.,-ROUT, ZOUT)
        ENDIF
      ENDDO
      CALL JRCLOS
  999 RETURN
      END
