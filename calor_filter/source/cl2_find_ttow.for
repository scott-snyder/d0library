      SUBROUTINE CL2_FIND_TTOW(DATA,POINT,GOOD,PAST,START,LAST)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-      given a min and max value, find the location of data between these
C-      bounds.
C-
C-      The number of elements is in the location POINT, and the data is found
C-      starting after the location point.
C-      The data is assumed to be ordered in ascending values
C-
C-      The actual data assumed is 32bit raw calorimeter data; the address is
C-      in the high-order bytes; the raw data looks like
C-              16bit address * 2**16 + raw PH
C-      The search boundaries are for channel 0 of a given tower, so any real
C-      data will have some lower-order bits set; no exact matches are expected
C-
C-      A Binary Search is used
C-      (reference: Wirth, Algorithms + Data Structures = Programs, P 14)
C-
C-   Inputs  :
C-      DATA   the array containing calorimeter data
C-      POINT           pointer to 0th word of the adc data: format is
C-              DATA(POINT)    contains NDATA, the number of words to follow
C-              DATA(POINT+1)...DATA(POINT+NDATA)
C-                              contain the data to be searched
C-      GOOD           first valid address of data in region of interest
C-      PAST           first address greater than that of any data of interest
C-                      PAST must be > GOOD  (then 2nd search is smaller)
C-
C-   Outputs :
C-      START           location of first data in the region of interest
C-      LAST            location of LAST DATA IN THESE TOWERS
C-
C-    If LAST < START, no data found in region of interst
C-
C-   Controls: None
C-
C----------------------------------------------------------------------
C
C-   Created   18-MAY-1986  James T. Linnemann
C-   Updated   6-JAN-1991   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER DATA(*)
      INTEGER POINT,GOOD,PAST
      INTEGER START,LAST
      INTEGER NDATA                     ! nubmer of data words this ADC
      INTEGER LO,HI                     ! bounds of explored data on card
      INTEGER WANT                      ! value being searched for
      INTEGER TEST                      ! pointer being moved
C                                       ! towards the location of WANT
      INTEGER ENDPOINT     ! endpoint searched for (1ST for GOOD, 2nd for PAST)
C----------------------------------------------------------------------
C
C...find boundaries of data sought; search for each endpoint separately
      NDATA = DATA(POINT)
      DO ENDPOINT = 1,2                !1st good, 1st bad
        IF (ENDPOINT.EQ.1) THEN
          LO = POINT + 1
          HI = POINT + NDATA
          WANT = GOOD                 !seek 1st valid address
        ELSE
          HI = POINT + NDATA           !and keep old value of lo
          WANT = PAST                 !seek 1st invalid address
        ENDIF
C
C...Binary Search
C...Loop Invariant:
C...   DATA(POINT+1...LO-1) .lt. WANT         ("lower" set)
C...   DATA(HI+1...POINT+NDATA) .ge. WANT    ("upper" set)
C
C...the loop terminates with LO as the pointer to the lowest-valued data which
C       equals or exceeds the test value WANT; if no such, LO = POINT+NDATA+1

        DO WHILE (LO.LE.HI)
          TEST = (LO + HI)/2
          IF (DATA(TEST).LT.WANT) THEN
            LO = TEST + 1              !enlarge lower set
          ELSE
            HI = TEST - 1             !enlarge upper set
          ENDIF
        END DO
        IF (ENDPOINT.EQ.1) THEN         ! save results of search:
          START = LO                    ! first value > GOOD
        ELSE
          LAST = LO - 1                 !really want last good, not 1st bad
        ENDIF
      ENDDO
  999 RETURN
      END
