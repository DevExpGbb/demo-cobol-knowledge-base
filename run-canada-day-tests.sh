#!/bin/bash

# Canada Day Test Suite Runner
# Compiles and executes all test programs for Canada Day functionality

echo "======================================="
echo "CANADA DAY TEST SUITE RUNNER"
echo "======================================="
echo

# Set up environment
export COB_CC_FLAGS="-free"
export COB_LIBRARY_PATH="."

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "Step 1: Compiling Canada Day Check routine..."
if cobc -c canada-day-check.cbl; then
    echo -e "${GREEN}✓ canada-day-check compiled successfully${NC}"
else
    echo -e "${RED}✗ Failed to compile canada-day-check${NC}"
    exit 1
fi

echo
echo "Step 2: Compiling Original Test Suite..."
if cobc -x -o canada-day-test canada-day-test.cbl canada-day-check.o; then
    echo -e "${GREEN}✓ canada-day-test compiled successfully${NC}"
else
    echo -e "${RED}✗ Failed to compile canada-day-test${NC}"
    exit 1
fi

echo
echo "Step 3: Compiling Comprehensive Test Suite..."
if cobc -x -o canada-day-comprehensive-test canada-day-comprehensive-test.cbl canada-day-check.o; then
    echo -e "${GREEN}✓ canada-day-comprehensive-test compiled successfully${NC}"
else
    echo -e "${RED}✗ Failed to compile canada-day-comprehensive-test${NC}"
    exit 1
fi

echo
echo "======================================="
echo "RUNNING ORIGINAL TEST SUITE"
echo "======================================="
echo

if ./canada-day-test; then
    echo -e "${GREEN}✓ Original test suite completed${NC}"
else
    echo -e "${RED}✗ Original test suite failed${NC}"
fi

echo
echo "======================================="
echo "RUNNING COMPREHENSIVE TEST SUITE"
echo "======================================="
echo

if ./canada-day-comprehensive-test; then
    echo -e "${GREEN}✓ Comprehensive test suite completed${NC}"
else
    echo -e "${RED}✗ Comprehensive test suite failed${NC}"
fi

echo
echo "======================================="
echo "TEST SUITE EXECUTION COMPLETE"
echo "======================================="
echo
echo "Test files created:"
echo "- canada-day-test-data.cpy (Mock test data copybook)"
echo "- canada-day-comprehensive-test.cbl (Enhanced test suite)"
echo "- run-canada-day-tests.sh (This test runner script)"
echo
echo "Mock data includes:"
echo "- 20 comprehensive test scenarios"
echo "- Valid Canada Day dates (weekdays, weekends)"
echo "- Weekend observance rules testing"
echo "- Non-Canada Day dates"
echo "- Error condition testing"
echo "- Leap year validation"
echo "- Century boundary testing"
echo "- Input validation testing"
echo
echo "Use this script for regression testing as the code evolves."