# Canada Day Test Suite

This directory contains a comprehensive test suite for the Canada Day holiday determination functionality.

## Test Files

### Core Implementation
- `canada-day-check.cbl` - Main COBOL routine for Canada Day determination
- `canada-day-copybook.cpy` - Data structures and constants

### Test Programs
- `canada-day-test.cbl` - Original basic test suite (5 test cases)
- `canada-day-comprehensive-test.cbl` - Enhanced test suite (20 test cases)
- `canada-day-test-data.cpy` - Mock test data copybook with comprehensive scenarios

### Test Runner
- `run-canada-day-tests.sh` - Script to compile and run all tests

## Running the Tests

To run all tests:
```bash
./run-canada-day-tests.sh
```

This will:
1. Compile the Canada Day check routine
2. Compile both test programs
3. Run the original test suite
4. Run the comprehensive test suite
5. Display results for both

## Test Coverage

The comprehensive test suite includes:

### Valid Scenarios (11 tests)
- Canada Day on weekdays (observed on actual date)
- Canada Day on Saturday (observed on Friday)
- Canada Day on Sunday (observed on Monday)  
- Historical dates (1867 onwards)
- Leap year dates
- Century boundary dates

### Error Scenarios (5 tests)
- Invalid years (before 1867)
- Invalid months (13, etc.)
- Invalid days (32, February 29 on non-leap years)
- Comprehensive date validation

### Edge Cases (4 tests)
- Non-Canada Day dates
- Boundary dates (June 30, July 2)
- Different observance flag settings
- Weekend patterns across multiple years

## Mock Data Features

The test data is:
- **Reusable**: Stored in copybook for easy maintenance
- **Comprehensive**: Covers all major scenarios and edge cases
- **Maintainable**: Easy to add new test cases
- **Documented**: Each test case has descriptive names
- **Regression-ready**: Can be run repeatedly as code evolves

## Test Results Interpretation

- **PASS**: Test case executed successfully and all results match expected values
- **FAIL**: One or more results don't match expected values (detailed output shows differences)

The test suite validates:
- Canada Day flag (Y/N)
- Observed date calculation
- Return codes
- Error messages

## Adding New Test Cases

To add new test cases:

1. Edit `canada-day-test-data.cpy`
2. Add a new `TD-CASE-XXX` entry following the existing pattern
3. Update `MAX-TEST-CASES` constant
4. Recompile and run tests

Example test case structure:
```cobol
05  TD-CASE-021.
    10  FILLER PIC X(50) VALUE 'Test description'.
    10  FILLER PIC 9(8) VALUE 20250701.  * Input date
    10  FILLER PIC X(1) VALUE 'Y'.       * Observance flag
    10  FILLER PIC X(1) VALUE 'Y'.       * Expected Canada flag
    10  FILLER PIC 9(8) VALUE 20250701.  * Expected observed date
    10  FILLER PIC 9(2) VALUE 00.        * Expected return code
    10  FILLER PIC X(40) VALUE SPACES.   * Expected error message
```

This test suite provides a solid foundation for maintaining code quality and ensuring the Canada Day determination logic continues to work correctly as the codebase evolves.