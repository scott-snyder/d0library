C VAX/DEC CMS REPLACEMENT HISTORY, Element KT_MERGE_CELLS.FOR
C *1     3-FEB-1994 14:41:42 FRAME "catani jet algorithm"
C VAX/DEC CMS REPLACEMENT HISTORY, Element KT_MERGE_CELLS.FOR
      SUBROUTINE KT_MERGE_CELLS( I1DUM, I2DUM )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : This is a utility routine for the KTJET routines.
C-                         We will attempt to combine these two clusters.
C-                         Combine the 4-vectors, merge them into one cluster
C-                         with common CLASS = I1
C-
C-  ENTRY KT_SPLIT_CELLS( I1DUM, I2DUM )
C-                       : Split two 4 vectors that were once combined. If
C-                       : so, the convention is that the greater number
C-                       : was combined into the lesser. So we just need to
C-                       : use the greater to determine what the lesser used
C-                       : to be. Note that both splitting and merging will
C-                       : depend on the ET,eta,phi merging scheme used.
C-                       : It is not possible to correctly split 4-vectors
C-                       : which were merged under ET=Esin(theta) and ET
C-                       : weighted eta,phi. A warning is issued in this case.
C-
C-  ENTRY KT_MERGED_CELLS( DONE )
C-                       : DONE is TRUE if KT_MERGE_CELLS has been called
C-                       : since the last reset
C-
C-  ENTRY KT_MERGED_RESET
C-                       : Reset
C-
C-   Inputs  : [I]   I1 : Pointer to the KVEC bank for first cluster
C-             [I]   I2 : Pointer to the KVEC bank for second cluster
C-
C-   Outputs :
C-   Controls:
C-
C-   Created  16-AUG-1992   Richard V. Astur
C-   Modified 25-JUL-1995   RA "Bug in split cells with IETDEF=2 & IETADEF=1"
c-   Updated  07-Nov-1995   Use errmsg instead of type *
c-                          Gordon Watts (Brown)
c-   Updated  15-Dec-1995   Fixed up format descriptors so this will build on
c-                          the IBMs.
c-                          Gordon Watts (Brown)
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:KTJET.INC'
      INCLUDE 'D0$INC:KTJET_LINK.INC'
      INTEGER I1, I2, NEW_CLASS, INEXT1, INEXT2, ICLASS
      INTEGER I1DUM, I2DUM, I11, I22
      INTEGER J1DUM, J2DUM
      REAL KT2_MAX, KT3_MAX, THETA, ETWGT
      LOGICAL FIRST_SPLIT
      LOGICAL CALLED, RET_CALLED
      SAVE CALLED
      DATA CALLED /.FALSE./
      SAVE FIRST_SPLIT
      DATA FIRST_SPLIT /.TRUE./
      REAL RPHI1,RPHI2,DELTA_PHI

      character*80 line

      INCLUDE 'D0$INC:KTJET_UTIL.DEF'
C----------------------------------------------------------------------
C STATEMENT FUNCTION
C
C---Relative Delta Phi. Equivalent to RPHI2-RPHI1 but take into account
C---wrap around to give smallest absolute value.
C
      DELTA_PHI(RPHI1,RPHI2) =
     &  MIN(MOD(ABS((RPHI1)-(RPHI2)),2*SNGL(PI)),
     &  2*SNGL(PI)-MOD(ABS((RPHI1)-(RPHI2)),2*SNGL(PI)))*
     &  SIGN(1.,(RPHI2)-(RPHI1))*((-1)**INT(((RPHI2)-(RPHI1))/SNGL(PI)))

C----------------------------------------------------------------------
C
C: Set flag that says we were called at least once
C
      CALLED = .TRUE.
C
C: Always merge into the smaller of I1DUM and I2DUM
C
      I1 = MIN( I1DUM, I2DUM )
      I2 = MAX( I1DUM, I2DUM )
C
C: Combine the 4 vectors of these two, and place in I1
C
      Q(PX( I1 )) = Q(PX( I1 )) + Q(PX( I2 ))
      Q(PY( I1 )) = Q(PY( I1 )) + Q(PY( I2 ))
      Q(PZ( I1 )) = Q(PZ( I1 )) + Q(PZ( I2 ))
      Q(P0( I1 )) = Q(P0( I1 )) + Q(P0( I2 ))
C
C: Decide how to define the new ET,ETA,PHI
C
      IF ( IETADEF .EQ. 1 ) THEN  ! E WEIGHTED
        CALL ETOETA( Q(PX( I1 )), Q( KTPHI(I1) ), THETA, Q( KTETA(I1) )
     &    )
      ELSEIF (IETADEF .EQ. 2 ) THEN
        Q( KTETA(I1) ) = Q(KTETA(I1)) + ( Q(KTETA(I2)) - Q(KTETA(I1)) )*
     &    Q(KTET(I2))/(Q(KTET(I1))+Q(KTET(I2)))
        THETA = 2*ATAN(EXP(-Q(KTETA(I1))))
        Q( KTPHI(I1))  = Q( KTPHI(I1) ) + DELTA_PHI(Q(KTPHI(I1)),
     &    Q(KTPHI(I2))) * Q( KTET(I2))/(Q(KTET(I1))+Q(KTET(I2)))
      ELSE
        CALL ERRMSG('Illegal IETADEF','KT_MERGE_CELLS',
     &    'IETADEF out of range', 'F')
      ENDIF
      IF ( Q( KTPHI(I1)) .GT. TWOPI ) Q( KTPHI(I1)) = Q( KTPHI(I1)) -
     &  TWOPI
      IF ( Q( KTPHI(I1)) .LT. 0. ) Q( KTPHI(I1)) = Q( KTPHI(I1)) +
     &  TWOPI

      IF ( IETDEF .EQ. 1 ) THEN   ! ETVECT
        Q(KTET(I1)) = SQRT(Q(PX(I1))**2 + Q(PY(I1))**2)
      ELSEIF ( IETDEF .EQ. 2 ) THEN ! ESINTHETA
        Q(KTET(I1)) = Q(P0(I1))*SIN(THETA)
      ELSEIF ( IETDEF .EQ. 3 ) THEN ! ETSUM
        Q(KTET(I1)) = Q(KTET(I1)) + Q(KTET(I2))
      ELSE
        CALL ERRMSG('Illegal IETDEF','KT_MERGE_CELLS',
     &    'IETDEF out of range', 'F')
      ENDIF

C
C: Link the two clusters together
C: Set KVEC pointer negative for I2
C

      I11     = IQ( POINT( I1 ) )
      I22     = IQ( POINT( I2 ) )
      INEXT1 = IQ( NEXT_MAP( I11 ))                 ! Set pointers to form
      INEXT2 = IQ( NEXT_MAP( I22 ))                 ! one circle from the two
      IQ( NEXT_MAP( I11 )) = INEXT2                 ! we have now.
      IQ( NEXT_MAP( I22 )) = INEXT1

      IQ( POINT( I2 )) = -IQ( POINT( I2 ))
      IF(INEXT1.EQ.0.OR.INEXT2.EQ.0)THEN
         write (line, '(a,i2,a,i7,a,i2)') 'INPUT_TYPE',INPUT_TYPE,
     $        'EVENT#', IQ(LHEAD+9),' IKTCUT',
     &        IKTCUT
         call errmsg ('bad-link-0', 'kt_merge_cells', line, 'w')
        write (line, '(a,i8, a, i8, a, i8, a, i8, a, i8)')'I1',I1,
     $        'INEXT1',INEXT1,' IQ(NEXT_MAP(I11))',
     &        IQ(NEXT_MAP(I11)),' I2',I2,' INEXT2',INEXT2,
     &        ' IQ(NEXT_MAP(I22))',IQ(NEXT_MAP(I22))
         call errmsg ('bad-link-1', 'kt_merge_cells', line, 'w')
      ENDIF

  999 RETURN

C------------------------------------------------------------------------
C- KT_SPLIT_CELLS : split the two 4 vectors again
C------------------------------------------------------------------------
      ENTRY KT_SPLIT_CELLS( J1DUM, J2DUM )

C
C: Always merge into the smaller of J1DUM and J2DUM
C: So I1 is the one that got merged into.
C
      I1 = MIN( J1DUM, J2DUM )
      I2 = MAX( J1DUM, J2DUM )
C
C: Combine the 4 vectors of these two, and place in I1
C
      Q(PX( I1 )) = Q(PX( I1 )) - Q(PX( I2 ))
      Q(PY( I1 )) = Q(PY( I1 )) - Q(PY( I2 ))
      Q(PZ( I1 )) = Q(PZ( I1 )) - Q(PZ( I2 ))
      Q(P0( I1 )) = Q(P0( I1 )) - Q(P0( I2 ))
C
C: Decide how to define the new ET,ETA,PHI
C
C: ET
      IF ( IETDEF .EQ. 1 ) THEN                   ! ETVECT
        Q(KTET(I1)) = SQRT(Q(PX(I1))**2 + Q(PY(I1))**2)
      ELSEIF ( IETDEF .EQ. 2 ) THEN               ! ESINTHETA
        Q(KTET(I1)) = Q(KTET(I1)) - Q(KTET(I2))   ! Temp. This works best
      ELSEIF ( IETDEF .EQ. 3 ) THEN ! ETSUM       ! ETSUM
        Q(KTET(I1)) = Q(KTET(I1)) - Q(KTET(I2))
      ELSE
        CALL ERRMSG('Illegal IETDEF','KT_SPLIT_CELLS',
     &    'IETDEF out of range', 'F')
      ENDIF
C
C: ETA
C
      IF ( IETADEF .EQ. 1 ) THEN  ! E WEIGHTED
        CALL ETOETA( Q(PX( I1 )), Q( KTPHI(I1) ), THETA, Q( KTETA(I1) )
     &    )
C
C: Final fix of ET for IET=1, IETA=2
C
        IF ( IETDEF .EQ. 2 ) THEN
          Q(KTET(I1)) = Q(P0(I1))*SIN(THETA)
        ENDIF
      ELSEIF (IETADEF .EQ. 2 ) THEN
        ETWGT = Q(KTET(I2))/( Q(KTET(I1)) + Q(KTET(I2)) )
        Q( KTETA(I1) ) = ( Q(KTETA(I1)) - Q(KTETA(I2)) + Q(KTETA(I2)) -
     &    Q(KTETA(I2))*ETWGT )/(1.-ETWGT)
        THETA = 2*ATAN(EXP(-Q(KTETA(I1))))
        Q( KTPHI(I1))  = ( DELTA_PHI( Q(KTPHI(I2)), Q(KTPHI(I1)) ) +
     &    Q(KTPHI(I2)) - Q(KTPHI(I2))*ETWGT)/(1.-ETWGT)
      
        IF ( IETDEF .EQ. 2 .AND. IETADEF .EQ. 2 ) THEN
          IF ( FIRST_SPLIT ) THEN
            FIRST_SPLIT = .FALSE.
            CALL ERRMSG('SPLIT PROBLEM','KT_SPLIT_CELLS',
     &      'Cannot split exactly when IETDEF=2 AND IETADEF=2','W')
          ENDIF
          Q( KTET(I1)) = Q( P0(I1) )*SIN(THETA)
        ENDIF
      ELSE
        CALL ERRMSG('Illegal IETADEF','KT_SPLIT_CELLS',
     &    'IETADEF out of range', 'F')
      ENDIF
C
C: Map phi to range from 0 to twopi
C
      IF ( Q( KTPHI(I1)) .GT. TWOPI ) Q( KTPHI(I1)) = Q( KTPHI(I1)) -
     &  TWOPI
      IF ( Q( KTPHI(I1)) .LT. 0. ) Q( KTPHI(I1)) = Q( KTPHI(I1)) +
     &  TWOPI

C
C: Cluster I2 is valid again. Set its pointer positive
C
      IQ( POINT( I2 ) ) = -IQ( POINT(I2) )
C
C:  Split the two clusters
C
      I11 = IQ( POINT(I1) )
      I22 = IQ( POINT(I2) )
      INEXT1 = IQ( NEXT_MAP( I11 ))
      INEXT2 = IQ( NEXT_MAP( I22 ))
      IQ( NEXT_MAP( I11 ))  = INEXT2
      IQ( NEXT_MAP( I22 ))  = INEXT1

      RETURN


      ENTRY KT_MERGED_CELLS( RET_CALLED )
      RET_CALLED = CALLED
      RETURN

      ENTRY KT_MERGED_RESET
      CALLED = .FALSE.
      RETURN

      END
