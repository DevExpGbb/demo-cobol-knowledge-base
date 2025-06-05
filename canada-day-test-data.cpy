      ******************************************************************
      * COPYBOOK: CANADA-DAY-TEST-DATA                               *
      * PURPOSE:  Mock test data for Canada Day testing              *
      * AUTHOR:   Enterprise COBOL Development Team                  *
      * DATE:     2024                                               *
      * VERSION:  1.0                                                *
      ******************************************************************
      * DESCRIPTION:                                                 *
      * This copybook contains comprehensive test data for Canada    *
      * Day determination testing, including valid scenarios, edge   *
      * cases, and error conditions.                                 *
      ******************************************************************

      * Test case structure
       01  TEST-CASE-STRUCTURE.
           05  TC-DESCRIPTION              PIC X(50).
           05  TC-INPUT-DATE               PIC 9(8).
           05  TC-OBSERVANCE-FLAG          PIC X(1).
           05  TC-EXPECTED-CANADA-FLAG     PIC X(1).
           05  TC-EXPECTED-OBSERVED-DATE   PIC 9(8).
           05  TC-EXPECTED-RETURN-CODE     PIC 9(2).
           05  TC-EXPECTED-ERROR-MSG       PIC X(40).

      * Test data table - comprehensive scenarios
       01  CANADA-DAY-TEST-DATA-TABLE.
      * Valid Canada Day scenarios
           05  TD-CASE-001.
               10  FILLER PIC X(50) VALUE 
                   'Canada Day 2024 - Monday (weekday)'.
               10  FILLER PIC 9(8) VALUE 20240701.
               10  FILLER PIC X(1) VALUE 'Y'.
               10  FILLER PIC X(1) VALUE 'Y'.
               10  FILLER PIC 9(8) VALUE 20240701.
               10  FILLER PIC 9(2) VALUE 00.
               10  FILLER PIC X(40) VALUE SPACES.
           05  TD-CASE-002.
               10  FILLER PIC X(50) VALUE 
                   'Canada Day 2023 - Saturday (observed Friday)'.
               10  FILLER PIC 9(8) VALUE 20230701.
               10  FILLER PIC X(1) VALUE 'Y'.
               10  FILLER PIC X(1) VALUE 'Y'.
               10  FILLER PIC 9(8) VALUE 20230630.
               10  FILLER PIC 9(2) VALUE 00.
               10  FILLER PIC X(40) VALUE SPACES.
           05  TD-CASE-003.
               10  FILLER PIC X(50) VALUE 
                   'Canada Day 2029 - Sunday (observed Monday)'.
               10  FILLER PIC 9(8) VALUE 20290701.
               10  FILLER PIC X(1) VALUE 'Y'.
               10  FILLER PIC X(1) VALUE 'Y'.
               10  FILLER PIC 9(8) VALUE 20290702.
               10  FILLER PIC 9(2) VALUE 00.
               10  FILLER PIC X(40) VALUE SPACES.
           05  TD-CASE-004.
               10  FILLER PIC X(50) VALUE 
                   'Canada Day 2025 - Tuesday (weekday)'.
               10  FILLER PIC 9(8) VALUE 20250701.         
               10  FILLER PIC X(1) VALUE 'Y'.
               10  FILLER PIC X(1) VALUE 'Y'.
               10  FILLER PIC 9(8) VALUE 20250701.
               10  FILLER PIC 9(2) VALUE 00.
               10  FILLER PIC X(40) VALUE SPACES.
           05  TD-CASE-005.
               10  FILLER PIC X(50) VALUE 
                   'Canada Day 1867 - Confederation year'.
               10  FILLER PIC 9(8) VALUE 18670701.
               10  FILLER PIC X(1) VALUE 'Y'.
               10  FILLER PIC X(1) VALUE 'Y'.
               10  FILLER PIC 9(8) VALUE 18670701.
               10  FILLER PIC 9(2) VALUE 00.
               10  FILLER PIC X(40) VALUE SPACES.
      * Non-observance scenarios
           05  TD-CASE-006.
               10  FILLER PIC X(50) VALUE 
                   'Canada Day 2023 - Saturday (no observance)'.
               10  FILLER PIC 9(8) VALUE 20230701.
               10  FILLER PIC X(1) VALUE 'N'.
               10  FILLER PIC X(1) VALUE 'Y'.
               10  FILLER PIC 9(8) VALUE 20230701.
               10  FILLER PIC 9(2) VALUE 00.
               10  FILLER PIC X(40) VALUE SPACES.
      * Century boundaries
           05  TD-CASE-007.
               10  FILLER PIC X(50) VALUE 
                   'Canada Day 2000 - Y2K leap year Saturday'.
               10  FILLER PIC 9(8) VALUE 20000701.
               10  FILLER PIC X(1) VALUE 'Y'.
               10  FILLER PIC X(1) VALUE 'Y'.
               10  FILLER PIC 9(8) VALUE 20000630.
               10  FILLER PIC 9(2) VALUE 00.
               10  FILLER PIC X(40) VALUE SPACES.
           05  TD-CASE-008.
               10  FILLER PIC X(50) VALUE 
                   'Canada Day 1900 - Century non-leap year'.
               10  FILLER PIC 9(8) VALUE 19000701.
               10  FILLER PIC X(1) VALUE 'Y'.
               10  FILLER PIC X(1) VALUE 'Y'.
               10  FILLER PIC 9(8) VALUE 19000702.
               10  FILLER PIC 9(2) VALUE 00.
               10  FILLER PIC X(40) VALUE SPACES.
      * Non-Canada Day scenarios
           05  TD-CASE-009.
               10  FILLER PIC X(50) VALUE 
                   'Independence Day USA - July 4th'.
               10  FILLER PIC 9(8) VALUE 20240704.
               10  FILLER PIC X(1) VALUE 'Y'.
               10  FILLER PIC X(1) VALUE 'N'.
               10  FILLER PIC 9(8) VALUE 20240704.
               10  FILLER PIC 9(2) VALUE 00.
               10  FILLER PIC X(40) VALUE SPACES.
           05  TD-CASE-010.
               10  FILLER PIC X(50) VALUE 
                   'June 30th - Day before Canada Day'.
               10  FILLER PIC 9(8) VALUE 20240630.
               10  FILLER PIC X(1) VALUE 'Y'.
               10  FILLER PIC X(1) VALUE 'N'.
               10  FILLER PIC 9(8) VALUE 20240630.
               10  FILLER PIC 9(2) VALUE 00.
               10  FILLER PIC X(40) VALUE SPACES.
           05  TD-CASE-011.
               10  FILLER PIC X(50) VALUE 
                   'July 2nd - Day after Canada Day'.
               10  FILLER PIC 9(8) VALUE 20240702.
               10  FILLER PIC X(1) VALUE 'Y'.
               10  FILLER PIC X(1) VALUE 'N'.
               10  FILLER PIC 9(8) VALUE 20240702.
               10  FILLER PIC 9(2) VALUE 00.
               10  FILLER PIC X(40) VALUE SPACES.
      * Error scenarios - Invalid dates
           05  TD-CASE-012.
               10  FILLER PIC X(50) VALUE 
                   'Error: Year before Confederation'.
               10  FILLER PIC 9(8) VALUE 18660701.
               10  FILLER PIC X(1) VALUE 'Y'.
               10  FILLER PIC X(1) VALUE 'N'.
               10  FILLER PIC 9(8) VALUE 00000000.
               10  FILLER PIC 9(2) VALUE 02.
               10  FILLER PIC X(40) VALUE 
                   'Invalid year - must be 1867 or later'.
           05  TD-CASE-013.
               10  FILLER PIC X(50) VALUE 
                   'Error: Invalid month 13'.
               10  FILLER PIC 9(8) VALUE 20241301.
               10  FILLER PIC X(1) VALUE 'Y'.
               10  FILLER PIC X(1) VALUE 'N'.
               10  FILLER PIC 9(8) VALUE 00000000.
               10  FILLER PIC 9(2) VALUE 03.
               10  FILLER PIC X(40) VALUE 
                   'Invalid month - must be 01-12'.
           05  TD-CASE-014.
               10  FILLER PIC X(50) VALUE 
                   'Error: Invalid day February 29 non-leap'.
               10  FILLER PIC 9(8) VALUE 20230229.
               10  FILLER PIC X(1) VALUE 'Y'.
               10  FILLER PIC X(1) VALUE 'N'.
               10  FILLER PIC 9(8) VALUE 00000000.
               10  FILLER PIC 9(2) VALUE 04.
               10  FILLER PIC X(40) VALUE 
                   'Invalid day for given month and year'.
           05  TD-CASE-015.
               10  FILLER PIC X(50) VALUE 
                   'Error: Invalid day 32 for month'.
               10  FILLER PIC 9(8) VALUE 20240732.
               10  FILLER PIC X(1) VALUE 'Y'.
               10  FILLER PIC X(1) VALUE 'N'.
               10  FILLER PIC 9(8) VALUE 00000000.
               10  FILLER PIC 9(2) VALUE 04.
               10  FILLER PIC X(40) VALUE 
                   'Invalid day for given month and year'.
      * Leap year validation
           05  TD-CASE-016.
               10  FILLER PIC X(50) VALUE 
                   'Valid: February 29 leap year 2024'.
               10  FILLER PIC 9(8) VALUE 20240229.
               10  FILLER PIC X(1) VALUE 'Y'.
               10  FILLER PIC X(1) VALUE 'N'.
               10  FILLER PIC 9(8) VALUE 20240229.
               10  FILLER PIC 9(2) VALUE 00.
               10  FILLER PIC X(40) VALUE SPACES.
           05  TD-CASE-017.
               10  FILLER PIC X(50) VALUE 
                   'Valid: February 29 leap year 2000'.
               10  FILLER PIC 9(8) VALUE 20000229.
               10  FILLER PIC X(1) VALUE 'Y'.
               10  FILLER PIC X(1) VALUE 'N'.
               10  FILLER PIC 9(8) VALUE 20000229.
               10  FILLER PIC 9(2) VALUE 00.
               10  FILLER PIC X(40) VALUE SPACES.
           05  TD-CASE-018.
               10  FILLER PIC X(50) VALUE 
                   'Error: February 29 non-leap year 1900'.
               10  FILLER PIC 9(8) VALUE 19000229.
               10  FILLER PIC X(1) VALUE 'Y'.
               10  FILLER PIC X(1) VALUE 'N'.
               10  FILLER PIC 9(8) VALUE 00000000.
               10  FILLER PIC 9(2) VALUE 04.
               10  FILLER PIC X(40) VALUE 
                   'Invalid day for given month and year'.
      * Weekend patterns for multiple years
           05  TD-CASE-019.
               10  FILLER PIC X(50) VALUE 
                   'Canada Day 2028 - Saturday pattern'.
               10  FILLER PIC 9(8) VALUE 20280701.
               10  FILLER PIC X(1) VALUE 'Y'.
               10  FILLER PIC X(1) VALUE 'Y'.
               10  FILLER PIC 9(8) VALUE 20280630.
               10  FILLER PIC 9(2) VALUE 00.
               10  FILLER PIC X(40) VALUE SPACES.
           05  TD-CASE-020.
               10  FILLER PIC X(50) VALUE 
                   'Canada Day 2030 - Monday pattern'.
               10  FILLER PIC 9(8) VALUE 20300701.
               10  FILLER PIC X(1) VALUE 'Y'.
               10  FILLER PIC X(1) VALUE 'Y'.
               10  FILLER PIC 9(8) VALUE 20300701.
               10  FILLER PIC 9(2) VALUE 00.
               10  FILLER PIC X(40) VALUE SPACES.

       01  TEST-DATA-ARRAY REDEFINES CANADA-DAY-TEST-DATA-TABLE.
           05  TEST-CASE OCCURS 20 TIMES.
               10  TCA-DESCRIPTION         PIC X(50).
               10  TCA-INPUT-DATE          PIC 9(8).
               10  TCA-OBSERVANCE-FLAG     PIC X(1).
               10  TCA-EXP-CANADA-FLAG     PIC X(1).
               10  TCA-EXP-OBSERVED-DATE   PIC 9(8).
               10  TCA-EXP-RETURN-CODE     PIC 9(2).
               10  TCA-EXP-ERROR-MSG       PIC X(40).

      * Test constants
       01  TEST-CONSTANTS.
           05  MAX-TEST-CASES              PIC 9(2) VALUE 20.
           05  TEST-PASSED                 PIC X(4) VALUE 'PASS'.
           05  TEST-FAILED                 PIC X(4) VALUE 'FAIL'.
