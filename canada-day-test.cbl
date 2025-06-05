      ******************************************************************
      * PROGRAM: CANADA-DAY-TEST                                      *
      * PURPOSE: Test driver for Canada Day determination routine     *
      * AUTHOR:  Enterprise COBOL Development Team                    *
      * DATE:    2024                                                 *
      * VERSION: 1.0                                                  *
      ******************************************************************
      * DESCRIPTION:                                                  *
      * This program demonstrates usage of the CANADA-DAY-CHECK       *
      * routine with various test scenarios including valid dates,    *
      * weekend observance, and error conditions.                    *
      ******************************************************************
       
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CANADA-DAY-TEST.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-Z.
       OBJECT-COMPUTER. IBM-Z.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      * Test case counter
       01  WS-TEST-COUNTER                 PIC 9(3) VALUE 0.
       01  WS-PASS-COUNTER                 PIC 9(3) VALUE 0.
       01  WS-FAIL-COUNTER                 PIC 9(3) VALUE 0.
       
      * Test case data - memory optimized with reusable structure
       01  WS-CURRENT-TEST-CASE.
           05  WS-TC-DESC                  PIC X(40).
           05  WS-TC-INPUT-DATE            PIC 9(8).
           05  WS-TC-OBSERVANCE            PIC X(1).
           05  WS-TC-EXP-FLAG              PIC X(1).
           05  WS-TC-EXP-OBSERVED          PIC 9(8).
           05  WS-TC-EXP-RC                PIC 9(2).

      * Test data table - reduces memory footprint
       01  WS-TEST-DATA-TABLE.
           05  WS-TD-CASE-01.
               10  FILLER                  PIC X(40) 
                   VALUE 'Canada Day 2024 - Monday'.
               10  FILLER                  PIC 9(8) VALUE 20240701.
               10  FILLER                  PIC X(1) VALUE 'Y'.
               10  FILLER                  PIC X(1) VALUE 'Y'.
               10  FILLER                  PIC 9(8) VALUE 20240701.
               10  FILLER                  PIC 9(2) VALUE 00.
           05  WS-TD-CASE-02.
               10  FILLER                  PIC X(40)
                   VALUE 'Canada Day 2023 - Saturday'.
               10  FILLER                  PIC 9(8) VALUE 20230701.
               10  FILLER                  PIC X(1) VALUE 'Y'.
               10  FILLER                  PIC X(1) VALUE 'Y'.
               10  FILLER                  PIC 9(8) VALUE 20230630.
               10  FILLER                  PIC 9(2) VALUE 00.
           05  WS-TD-CASE-03.
               10  FILLER                  PIC X(40)
                   VALUE 'Canada Day 2029 - Sunday'.
               10  FILLER                  PIC 9(8) VALUE 20290701.
               10  FILLER                  PIC X(1) VALUE 'Y'.
               10  FILLER                  PIC X(1) VALUE 'Y'.
               10  FILLER                  PIC 9(8) VALUE 20290702.
               10  FILLER                  PIC 9(2) VALUE 00.
           05  WS-TD-CASE-04.
               10  FILLER                  PIC X(40)
                   VALUE 'Not Canada Day - July 4th'.
               10  FILLER                  PIC 9(8) VALUE 20240704.
               10  FILLER                  PIC X(1) VALUE 'Y'.
               10  FILLER                  PIC X(1) VALUE 'N'.
               10  FILLER                  PIC 9(8) VALUE 20240704.
               10  FILLER                  PIC 9(2) VALUE 00.
           05  WS-TD-CASE-05.
               10  FILLER                  PIC X(40)
                   VALUE 'Invalid year - before Confederation'.
               10  FILLER                  PIC 9(8) VALUE 18660701.
               10  FILLER                  PIC X(1) VALUE 'Y'.
               10  FILLER                  PIC X(1) VALUE 'N'.
               10  FILLER                  PIC 9(8) VALUE 00000000.
               10  FILLER                  PIC 9(2) VALUE 02.

       01  WS-TEST-CASE-ARRAY REDEFINES WS-TEST-DATA-TABLE.
           05  WS-TEST-CASE                OCCURS 5 TIMES.
               10  WS-TCA-DESC             PIC X(40).
               10  WS-TCA-INPUT-DATE       PIC 9(8).
               10  WS-TCA-OBSERVANCE       PIC X(1).
               10  WS-TCA-EXP-FLAG         PIC X(1).
               10  WS-TCA-EXP-OBSERVED     PIC 9(8).
               10  WS-TCA-EXP-RC           PIC 9(2).
       
      * Output from called program - reduced size
       01  WS-OUTPUT-PARAMS.
           05  WS-CANADA-DAY-FLAG          PIC X(1).
           05  WS-OBSERVED-DATE            PIC 9(8).
           05  WS-RETURN-CODE              PIC 9(2).
           05  WS-ERROR-MESSAGE            PIC X(40).

      * Loop control
       01  WS-TEST-INDEX                   PIC 9(1).
       
      * Display formatting
       01  WS-DISPLAY-LINE                 PIC X(80).
       
       PROCEDURE DIVISION.
       
      ******************************************************************
      * MAIN TEST PROCESSING                                          *
      ******************************************************************
       0000-MAIN-PROCESSING.
           DISPLAY 'CANADA DAY DETERMINATION - TEST SUITE'
           DISPLAY '======================================='
           DISPLAY ' '
           
           PERFORM VARYING WS-TEST-INDEX FROM 1 BY 1
                   UNTIL WS-TEST-INDEX > 5
               PERFORM 1000-RUN-SINGLE-TEST-CASE
           END-PERFORM
           
           PERFORM 9000-DISPLAY-TEST-SUMMARY
           STOP RUN.
       
      ******************************************************************
      * RUN SINGLE TEST CASE USING ARRAY INDEX                      *
      ******************************************************************
       1000-RUN-SINGLE-TEST-CASE.
           MOVE WS-TEST-CASE(WS-TEST-INDEX) TO WS-CURRENT-TEST-CASE
           
           CALL 'CANADA-DAY-CHECK' USING WS-TC-INPUT-DATE
                                         WS-TC-OBSERVANCE
                                         WS-CANADA-DAY-FLAG
                                         WS-OBSERVED-DATE
                                         WS-RETURN-CODE
                                         WS-ERROR-MESSAGE
           
           PERFORM 2000-VALIDATE-RESULTS.
       
      ******************************************************************
      * VALIDATE TEST RESULTS                                        *
      ******************************************************************
       2000-VALIDATE-RESULTS.
           ADD 1 TO WS-TEST-COUNTER
           
           IF WS-CANADA-DAY-FLAG = WS-TC-EXP-FLAG AND
              WS-OBSERVED-DATE = WS-TC-EXP-OBSERVED AND
              WS-RETURN-CODE = WS-TC-EXP-RC
               ADD 1 TO WS-PASS-COUNTER
               DISPLAY 'Test ' WS-TEST-COUNTER ': PASS - ' WS-TC-DESC
           ELSE
               ADD 1 TO WS-FAIL-COUNTER
               DISPLAY 'Test ' WS-TEST-COUNTER ': FAIL - ' WS-TC-DESC
               DISPLAY '  Expected: Flag=' WS-TC-EXP-FLAG 
                       ' Observed=' WS-TC-EXP-OBSERVED 
                       ' RC=' WS-TC-EXP-RC
               DISPLAY '  Actual:   Flag=' WS-CANADA-DAY-FLAG 
                       ' Obs=' WS-OBSERVED-DATE 
                       ' RC=' WS-RETURN-CODE
               IF WS-RETURN-CODE NOT = 00
                   DISPLAY '  Error: ' WS-ERROR-MESSAGE
               END-IF
           END-IF
           DISPLAY ' '.
       
      ******************************************************************
      * DISPLAY TEST SUMMARY                                         *
      ******************************************************************
       9000-DISPLAY-TEST-SUMMARY.
           DISPLAY '======================================='
           DISPLAY 'TEST SUMMARY:'
           DISPLAY 'Total Tests: ' WS-TEST-COUNTER
           DISPLAY 'Passed:      ' WS-PASS-COUNTER  
           DISPLAY 'Failed:      ' WS-FAIL-COUNTER
           
           IF WS-FAIL-COUNTER = 0
               DISPLAY 'ALL TESTS PASSED!'
           ELSE
               DISPLAY 'SOME TESTS FAILED!'
           END-IF.
