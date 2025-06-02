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
       
      * Test case data
       01  WS-TEST-CASES.
           05  WS-TEST-CASE-1.
               10  WS-TC1-DESC             PIC X(40) 
                   VALUE 'Canada Day 2024 - Monday'.
               10  WS-TC1-INPUT-DATE       PIC 9(8) VALUE 20240701.
               10  WS-TC1-OBSERVANCE       PIC X(1) VALUE 'Y'.
               10  WS-TC1-EXP-FLAG         PIC X(1) VALUE 'Y'.
               10  WS-TC1-EXP-OBSERVED     PIC 9(8) VALUE 20240701.
               10  WS-TC1-EXP-RC           PIC 9(2) VALUE 00.
           
           05  WS-TEST-CASE-2.
               10  WS-TC2-DESC             PIC X(40)
                   VALUE 'Canada Day 2023 - Saturday'.
               10  WS-TC2-INPUT-DATE       PIC 9(8) VALUE 20230701.
               10  WS-TC2-OBSERVANCE       PIC X(1) VALUE 'Y'.
               10  WS-TC2-EXP-FLAG         PIC X(1) VALUE 'Y'.
               10  WS-TC2-EXP-OBSERVED     PIC 9(8) VALUE 20230630.
               10  WS-TC2-EXP-RC           PIC 9(2) VALUE 00.
           
           05  WS-TEST-CASE-3.
               10  WS-TC3-DESC             PIC X(40)
                   VALUE 'Canada Day 2029 - Sunday'.
               10  WS-TC3-INPUT-DATE       PIC 9(8) VALUE 20290701.
               10  WS-TC3-OBSERVANCE       PIC X(1) VALUE 'Y'.
               10  WS-TC3-EXP-FLAG         PIC X(1) VALUE 'Y'.
               10  WS-TC3-EXP-OBSERVED     PIC 9(8) VALUE 20290702.
               10  WS-TC3-EXP-RC           PIC 9(2) VALUE 00.
           
           05  WS-TEST-CASE-4.
               10  WS-TC4-DESC             PIC X(40)
                   VALUE 'Not Canada Day - July 4th'.
               10  WS-TC4-INPUT-DATE       PIC 9(8) VALUE 20240704.
               10  WS-TC4-OBSERVANCE       PIC X(1) VALUE 'Y'.
               10  WS-TC4-EXP-FLAG         PIC X(1) VALUE 'N'.
               10  WS-TC4-EXP-OBSERVED     PIC 9(8) VALUE 20240704.
               10  WS-TC4-EXP-RC           PIC 9(2) VALUE 00.
           
           05  WS-TEST-CASE-5.
               10  WS-TC5-DESC             PIC X(40)
                   VALUE 'Invalid year - before Confederation'.
               10  WS-TC5-INPUT-DATE       PIC 9(8) VALUE 18660701.
               10  WS-TC5-OBSERVANCE       PIC X(1) VALUE 'Y'.
               10  WS-TC5-EXP-FLAG         PIC X(1) VALUE 'N'.
               10  WS-TC5-EXP-OBSERVED     PIC 9(8) VALUE 00000000.
               10  WS-TC5-EXP-RC           PIC 9(2) VALUE 02.
       
      * Output from called program
       01  WS-OUTPUT-PARAMS.
           05  WS-CANADA-DAY-FLAG          PIC X(1).
           05  WS-OBSERVED-DATE            PIC 9(8).
           05  WS-RETURN-CODE              PIC 9(2).
           05  WS-ERROR-MESSAGE            PIC X(50).
       
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
           
           PERFORM 1000-RUN-TEST-CASE-1
           PERFORM 1000-RUN-TEST-CASE-2  
           PERFORM 1000-RUN-TEST-CASE-3
           PERFORM 1000-RUN-TEST-CASE-4
           PERFORM 1000-RUN-TEST-CASE-5
           
           PERFORM 9000-DISPLAY-TEST-SUMMARY
           STOP RUN.
       
      ******************************************************************
      * RUN TEST CASE 1                                              *
      ******************************************************************
       1000-RUN-TEST-CASE-1.
           CALL 'CANADA-DAY-CHECK' USING WS-TC1-INPUT-DATE
                                         WS-TC1-OBSERVANCE
                                         WS-CANADA-DAY-FLAG
                                         WS-OBSERVED-DATE
                                         WS-RETURN-CODE
                                         WS-ERROR-MESSAGE
           
           PERFORM 2000-VALIDATE-RESULTS USING WS-TC1-DESC
                                               WS-TC1-EXP-FLAG
                                               WS-TC1-EXP-OBSERVED
                                               WS-TC1-EXP-RC.
       
      ******************************************************************
      * RUN TEST CASE 2                                              *
      ******************************************************************
       1000-RUN-TEST-CASE-2.
           CALL 'CANADA-DAY-CHECK' USING WS-TC2-INPUT-DATE
                                         WS-TC2-OBSERVANCE
                                         WS-CANADA-DAY-FLAG
                                         WS-OBSERVED-DATE
                                         WS-RETURN-CODE
                                         WS-ERROR-MESSAGE
           
           PERFORM 2000-VALIDATE-RESULTS USING WS-TC2-DESC
                                               WS-TC2-EXP-FLAG
                                               WS-TC2-EXP-OBSERVED
                                               WS-TC2-EXP-RC.
       
      ******************************************************************
      * RUN TEST CASE 3                                              *
      ******************************************************************
       1000-RUN-TEST-CASE-3.
           CALL 'CANADA-DAY-CHECK' USING WS-TC3-INPUT-DATE
                                         WS-TC3-OBSERVANCE
                                         WS-CANADA-DAY-FLAG
                                         WS-OBSERVED-DATE
                                         WS-RETURN-CODE
                                         WS-ERROR-MESSAGE
           
           PERFORM 2000-VALIDATE-RESULTS USING WS-TC3-DESC
                                               WS-TC3-EXP-FLAG
                                               WS-TC3-EXP-OBSERVED
                                               WS-TC3-EXP-RC.
       
      ******************************************************************
      * RUN TEST CASE 4                                              *
      ******************************************************************
       1000-RUN-TEST-CASE-4.
           CALL 'CANADA-DAY-CHECK' USING WS-TC4-INPUT-DATE
                                         WS-TC4-OBSERVANCE
                                         WS-CANADA-DAY-FLAG
                                         WS-OBSERVED-DATE
                                         WS-RETURN-CODE
                                         WS-ERROR-MESSAGE
           
           PERFORM 2000-VALIDATE-RESULTS USING WS-TC4-DESC
                                               WS-TC4-EXP-FLAG
                                               WS-TC4-EXP-OBSERVED
                                               WS-TC4-EXP-RC.
       
      ******************************************************************
      * RUN TEST CASE 5                                              *
      ******************************************************************
       1000-RUN-TEST-CASE-5.
           CALL 'CANADA-DAY-CHECK' USING WS-TC5-INPUT-DATE
                                         WS-TC5-OBSERVANCE
                                         WS-CANADA-DAY-FLAG
                                         WS-OBSERVED-DATE
                                         WS-RETURN-CODE
                                         WS-ERROR-MESSAGE
           
           PERFORM 2000-VALIDATE-RESULTS USING WS-TC5-DESC
                                               WS-TC5-EXP-FLAG
                                               WS-TC5-EXP-OBSERVED
                                               WS-TC5-EXP-RC.
       
      ******************************************************************
      * VALIDATE TEST RESULTS                                        *
      ******************************************************************
       2000-VALIDATE-RESULTS USING P-DESC P-EXP-FLAG P-EXP-OBS P-EXP-RC.
           ADD 1 TO WS-TEST-COUNTER
           
           IF WS-CANADA-DAY-FLAG = P-EXP-FLAG AND
              WS-OBSERVED-DATE = P-EXP-OBS AND
              WS-RETURN-CODE = P-EXP-RC
               ADD 1 TO WS-PASS-COUNTER
               DISPLAY 'Test ' WS-TEST-COUNTER ': PASS - ' P-DESC
           ELSE
               ADD 1 TO WS-FAIL-COUNTER
               DISPLAY 'Test ' WS-TEST-COUNTER ': FAIL - ' P-DESC
               DISPLAY '  Expected: Flag=' P-EXP-FLAG 
                       ' Observed=' P-EXP-OBS ' RC=' P-EXP-RC
               DISPLAY '  Actual:   Flag=' WS-CANADA-DAY-FLAG 
                       ' Observed=' WS-OBSERVED-DATE ' RC=' WS-RETURN-CODE
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