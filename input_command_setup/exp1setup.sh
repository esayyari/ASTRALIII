#!/bin/bash
#Experiment1 command and input file generator
DIR=/home/chaos/repository/Experiment1/
JAR3=/home/chaos/repository/ver3.jar
JAR2=/home/chaos/repository/ver2.jar

rm command
for j in 1X-1000-500-all.f200 2X-1000-500-all.f200 1X-1000-1500-all.f200
do
	shuf $j > temp
	for i in 1 2 3 4
	do
		shuf temp > temp2
		mv temp2 temp
		for k in 256 512 1024 2048 4096 8192 16384
		do
			head -$k temp > $j.$i.$k
			echo "java -jar ${JAR3} -t 0 -i ${DIR}$j.$i.$k -o ${DIR}$j.$i.$k.out3 &> ${DIR}$j.$i.$k.log3" >> command
			echo "java -jar ${JAR2} -t 0 -i ${DIR}$j.$i.$k -o ${DIR}$j.$i.$k.out2 &> ${DIR}$j.$i.$k.log2" >> command
		done
	done
done
