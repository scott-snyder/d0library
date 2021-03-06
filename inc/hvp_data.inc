C
C-- High Volatage Parameter Data --- 
C
      INTEGER POD_TYPE_ID
      INTEGER HV_ENABLE_TYPE 
      INTEGER HTS_TRIP_LIMIT 
      INTEGER HTS_PENDING_LIMIT 
      INTEGER HTS_STATE 
      INTEGER HTS_PENDING_COUNT 
      INTEGER HTS_COUNT 
      INTEGER HTS_TRIP_COUNT 
      INTEGER HTS_TRIP_DECAY_PERIOD 
      INTEGER PERIOD_FOR_MONITORING 
      INTEGER PERIOD_FOR_LOGGING 
      INTEGER ISPARE_1
      INTEGER ISPARE_2 
C
      CHARACTER*12 DEVICE_NAME
      CHARACTER*12 DEVICE_DESCRIPTOR 
      CHARACTER*12 DEVICE_GROUP 
      CHARACTER*12 DEVICE_SPARE 
C
      REAL CURRENT_LIMIT 
      REAL VOLTAGE_MARGIN 
      REAL RAMP_RATE 
      REAL TARGET_VOLTAGE 
      REAL HTS_MARGIN 
      REAL TOLERANCE_LIMIT_UPPER 
      REAL TOLERANCE_LIMIT_LOWER 
      REAL NOMINAL_VALUE 
      REAL WEIGHT_FOR_CURRENT
      REAL SPARE_1
      REAL SPARE_2 
      REAL SPARE_3 
      REAL SPARE_4 
C
      COMMON /HVP_DATA/POD_TYPE_ID,HV_ENABLE_TYPE,HTS_TRIP_LIMIT,
     1        HTS_PENDING_LIMIT,HTS_STATE,HTS_PENDING_COUNT,
     2        HTS_COUNT,HTS_TRIP_COUNT,HTS_TRIP_DECAY_PERIOD,
     3        PERIOD_FOR_MONITORING,PERIOD_FOR_LOGGING,
     4        ISPARE_1,ISPARE_2,DEVICE_NAME,DEVICE_DESCRIPTOR,
     5        DEVICE_GROUP,DEVICE_SPARE, 
     6        CURRENT_LIMIT,VOLTAGE_MARGIN,RAMP_RATE,TARGET_VOLTAGE,
     7        HTS_MARGIN,TOLERANCE_LIMIT_UPPER,TOLERANCE_LIMIT_LOWER,
     8        NOMINAL_VALUE,WEIGHT_FOR_CURRENT,
     9        SPARE_1,SPARE_2,SPARE_3,SPARE_4
C 
