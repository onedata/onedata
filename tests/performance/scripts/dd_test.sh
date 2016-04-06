#!/usr/bin/env bash

TEST_FILE=$1
TEST_TYPE=$2
blockSizes=

rm -rf ${TEST_FILE}

for size in 1024 102400 1048576 5242880 52428800
do
   for blockSize in 1 4 128 1024
   do
      for counter in {1..10}
      do
         blockCount=`expr $size / $blockSize`

         echo ""
         echo "File size: $size kB"
         echo "Block size: $blockSize kB"
         echo "Counter: $counter"
         echo ${TEST_TYPE}
         dd bs=${blockSize}k count=$blockCount if=/dev/zero of=${TEST_FILE} 2>&1
         dd bs=${blockSize}k count=$blockCount of=/dev/zero if=${TEST_FILE} 2>&1
         rm -rf ${TEST_FILE}
      done
   done
done

echo "Done"
