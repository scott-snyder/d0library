C----------------------------------------------------------------------
C-
C-   Created   3-OCT-1989   A.M.Jonckheere
C-
C----------------------------------------------------------------------
C
C ****  Sndtry    number of times to try sending data
C ****  Rdtry     number of times to try reading data for each send
C ****  icont     Logical .TRUE. continue to next device on fatal verify error
C ****  error_cnt count of number of retries
C ****  max_error max number of retries before quiting
C ****  no_heart  Logical .TRUE. run without heartbeat
C
c
      INTEGER max_itm
      PARAMETER( max_itm = 100 )
      INTEGER sndtry,rdtry,error_cnt(max_itm),max_error
      LOGICAL icont,no_heart
      COMMON /TKR_TRYS/ sndtry,rdtry,icont,error_cnt,max_error,no_heart
