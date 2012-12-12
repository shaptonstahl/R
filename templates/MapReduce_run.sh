#!/bin/bash

HDFS_OUTPUT_DIRECTORY=/home/hduser/wordcount-output

hadoop dfs -rmr $HDFS_OUTPUT_DIRECTORY

hadoop jar $HADOOP_STREAMING -input /home/hduser/gutenberg \
  -output $HDFS_OUTPUT_DIRECTORY \
  -mapper ~/wordcount/map.R \
  -reducer ~/wordcount/reduce.R
