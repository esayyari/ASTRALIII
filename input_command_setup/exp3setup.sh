#!/bin/bash
# Experiment3 input and command generator
DIR=/home/chaos/repository/Experiment3/
JAR3=/home/chaos/repository/ver3.jar
JAR2=/home/chaos/repository/ver2.jar

rm command1
rm command2
rm command3
for j in 01 02
do
	echo "java -Xmx45G -jar ${JAR} -t 0 -i ${DIR}10k.$j -o ${DIR}10k.$j.out3 &> ${DIR}10k.$j.log3 &" >> command1
	echo "java -Xmx15G -jar ${JAR} -t 0 -i ${DIR}5k.$j -o ${DIR}5k.$j.out3 &> ${DIR}5k.$j.log3 &" >> command1
done
echo "wait" >> command1
for j in 03 04
do
	echo "java -Xmx45G -jar ${JAR} -t 0 -i ${DIR}10k.$j -o ${DIR}10k.$j.out3 &> ${DIR}10k.$j.log3 &" >> command2
	echo "java -Xmx15G -jar ${JAR} -t 0 -i ${DIR}5k.$j -o ${DIR}5k.$j.out3 &> ${DIR}5k.$j.log3 &" >> command2
done
echo "wait" >> command2
for j in 01 02 03 04
do
	echo "java -Xmx10G -jar ${JAR} -t 0 -i ${DIR}2k.$j -o ${DIR}2k.$j.out3 &> ${DIR}2k.$j.log3 &" >> command3
done
echo "wait" >> command3