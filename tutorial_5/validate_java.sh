#!/usr/bin/env bash
#Execute with command: bash validate_java.sh > validation_result.txt

END=10
for ((i=1;i<=END;i++)); do
	java -jar uebung5-eval.jar validation/${i}fold_original.iob validation/${i}fold_prediction.iob
done