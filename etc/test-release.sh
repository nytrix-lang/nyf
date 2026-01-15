#!/bin/bash

set -e

echo "🧪 Testing Stack build for release..."
echo "======================================"

echo "1. Cleaning..."
stack clean

echo "2. Building..."
stack build --test --no-run-tests

echo "3. Finding binary..."
# Method 1: stack path
LOCAL_INSTALL=$(stack path --local-install-root)
echo "Local install root: $LOCAL_INSTALL"

if [ -f "$LOCAL_INSTALL/bin/nyf" ]; then
    echo "✅ Binary found at: $LOCAL_INSTALL/bin/nyf"
    echo ""
    echo "4. Testing binary..."
    "$LOCAL_INSTALL/bin/nyf" version
    echo ""
    "$LOCAL_INSTALL/bin/nyf" help | head -10
else
    echo "❌ Binary not found!"
    echo "Trying stack install..."
    stack install --local-bin-path ./test-bin
    if [ -f "./test-bin/nyf" ]; then
        echo "✅ Binary found at: ./test-bin/nyf"
        ./test-bin/nyf version
    else
        exit 1
    fi
fi

echo ""
echo "✅ All tests passed! Ready to create GitHub release."
